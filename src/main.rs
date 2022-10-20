use slint::{Model, VecModel, Color};
use std::{rc::Rc, cell::RefCell};
slint::include_modules!();

enum Orientation {
	Vertical,
	Horizontal,
}

impl Region {
    fn new (left: f32, right: f32, top: f32, bottom: f32, background: Option<Color>) -> Self {
        Self {
            left, right, top, bottom, 
            background : match background {
                Some(col) => col,
                _ => Color::from_rgb_f32(0.5, 0.5, 0.5)
            }
        }
    }
}
impl Edge {
    fn new (orientation: &Orientation, id: i32, cen: f32, start: f32, end: f32) -> Self {
        Self {
            isVert: matches!(orientation, Orientation::Horizontal),
            id, cen, start, end
        }
    }
}
struct UniFrame {
    min_size: f32,
    offset: f32,
    size: f32,
}
impl UniFrame {
    fn new(min_size: f32, offset: f32, size: f32) -> Self {
        Self {
            min_size,
            offset,
            size,
        }
    }
    fn far_drag_min(&self) -> f32 {
        self.min_size - self.size
    }
    fn near_drag_max(&self) -> f32 {
        self.size - self.min_size
    }
    fn drag_far(&mut self, drag: f32) {
        self.size += drag;
    }
    fn drag_near(&mut self, drag: f32) {
        self.offset += drag;
        self.size -= drag;
    }
    fn shrink(&mut self, dif: f32) -> f32 {
        let old_size = self.size;
        self.size -= dif;
        if self.size < self.min_size {
            self.size = self.min_size
        }
        old_size - self.size
    }
}

struct UniSplit {
	orientation: Orientation,
    frames: Vec<UniFrame>,
    length: f32,
    breadth: f32,
}
impl UniSplit {
	fn new(orientation: Orientation, length: f32, breadth: f32, min_size: f32, num: i32) -> Self {
        let avg_size = length/(num as f32);
		Self {
			orientation,
			frames: (0..num).map(|i| UniFrame::new(min_size, (i as f32)*avg_size, avg_size)).collect(),
            length,
            breadth,
		}
	}
    fn init_regions(&self) -> Vec<Region> {
        self.frames.iter().map(|uni| self.to_region(uni)).collect()
    }
    fn init_edges(&self) -> Vec<Edge> {
        self.frames.iter().enumerate().take(self.frames.len()-1)
            .map(|(i, frame)| self.to_far_edge(frame, i as i32)).collect()
    }
    fn to_region(&self, frame: &UniFrame) -> Region {
        match self.orientation {
            Orientation::Horizontal => Region::new(frame.offset, frame.offset+frame.size, 0.0, self.breadth, 
            Some(Color::from_rgb_f32(frame.offset/self.length, 0.5, 0.5))),
            Orientation::Vertical => Region::new(0.0, self.breadth, frame.offset, frame.offset+frame.size, 
            Some(Color::from_rgb_f32(frame.offset/self.length, 0.5, 0.5)))
        }
    }
    fn to_far_edge(&self, frame: &UniFrame, index: i32) -> Edge {
        Edge::new(&self.orientation, index, frame.offset+frame.size, 0.0, self.breadth)
    }
    fn align(&mut self) {
        let mut offset = 0.0;
        for frame in self.frames.iter_mut() {
            frame.offset = offset;
            offset += frame.size;
        }
    }
}
impl From<Rc<UniSplit>> for UniSplit {
    fn from(src: Rc<UniSplit>) -> Self {
        src.into()
    }
}
impl UniSplit {
    fn move_edge(&mut self, index: i32, drag: f32) -> bool {
        let index = index as usize;
        if let (Some(near), Some(far)) = (self.frames.get(index), self.frames.get(index+1)) {
            let min = near.far_drag_min();
            let max = far.near_drag_max();
            if min <= max {
                let drag = drag.clamp(min, max);
                self.frames[index].drag_far(drag);
                self.frames[index+1].drag_near(drag);
                return true;
            }
        }
        false
    }
    fn resize(&mut self, new_width: f32, new_height: f32) {
        //alt, store flex's instead of sizes, and then calculate size from outer box. cuz rn it seems like a small error typa issue.
            // have: percentage, new size -> new frames -> new percentages, and back around?
            // but what is the size from percentage calc? Well,take the ones that are too small, sub from the others, same idea.
        let (new_breadth, new_length) = match self.orientation {
            Orientation::Horizontal => (new_height, new_width),
            Orientation::Vertical => (new_width, new_height)
        };
        self.breadth = new_breadth;
        if self.length < new_length {
            let real_length = self.frames.iter().map(|f| f.size).sum::<f32>();
            if real_length < new_length {
                if real_length > self.length {
                    self.length = real_length;
                }
                let dif = new_length - self.length;
                for frame in self.frames.iter_mut() {
                    frame.size += dif*frame.size/self.length;
                }
                self.align();
            }
        } else if self.length > new_length {
            let mut rel_length = self.length;
            let mut exclude: Vec<usize> = Vec::new();
            while (rel_length - new_length).floor() > 0.0 && exclude.len() < self.frames.len() {
                let dif = rel_length - new_length;
                let mut new_rel_length = rel_length;
                for (i, frame) in self.frames.iter_mut().enumerate() {
                    if !exclude.contains(&i) {
                        let expected_dif = dif * frame.size/rel_length;
                        let shrink = frame.shrink(expected_dif);
                        new_rel_length -= shrink;
                        if shrink == 0.0 {
                            //println!("push {i}");
                            new_rel_length -= frame.size;
                            exclude.push(i);
                        }
                    } else {
                        //println!("exclude {i}");
                    }
                }
                rel_length = new_rel_length;
            }
            self.align();
        }
        self.length = new_length;
    }
}

struct ModelUpdator {
    regions: Rc<VecModel<Region>>,
    edges: Rc<VecModel<Edge>>,
}
impl ModelUpdator {
    fn init(model: &UniSplit) -> Self {
        Self {
            regions: Rc::new(VecModel::from(model.init_regions())),
            edges: Rc::new(VecModel::from(model.init_edges())),
        }
    }

    fn set_models_at_edge(&self, model: &UniSplit, index: i32) {
        let index = index as usize;
        if let (Some(near), Some(far)) = (model.frames.get(index), model.frames.get(index+1)) {
            self.regions.set_row_data(index, model.to_region(near));
            self.edges.set_row_data(index, model.to_far_edge(near, index as i32));
            self.regions.set_row_data(index+1, model.to_region(far));
        }
    }
    fn refresh_models(&self, model: &UniSplit) {
        for (index, region) in model.init_regions().into_iter().enumerate() {
            self.regions.set_row_data(index, region);
        }
        for (index, edge) in model.init_edges().into_iter().enumerate() {
            self.edges.set_row_data(index, edge);
        }
    }
}

// issue: can't initialize, when this is what creates the models...
// separate struct for updating? Easier to consolodate into model itself, later! (something like model.update(split.borrow(\\immutable)))
// seems messy to have UniFrame specific code in model updator... not like anything else would feed in. Make the name more spec or something...
// Option on UniFrame to associate updator?

fn main() {
    let win = MainWindow::new();

    let split_handle = Rc::new(RefCell::new(UniSplit::new(Orientation::Vertical, 800.0, 400.0, 100.0, 3)));
    let update_handle = Rc::new(ModelUpdator::init(&split_handle.borrow()));
    win.set_regions(update_handle.regions.clone().into());
    win.set_edges(update_handle.edges.clone().into());

    let dup_handles = || (Rc::clone(&split_handle), Rc::clone(&update_handle));
    {
        let (split_handle, update_handle) = dup_handles();
        win.on_resized(move |width, height| {
            split_handle.borrow_mut().resize(width, height);
            update_handle.refresh_models(&split_handle.borrow());
            true
        });
    }
    {
        let (split_handle, update_handle) = dup_handles();
        win.on_dragged(move |id, drag| {
            if split_handle.borrow_mut().move_edge(id, drag) {
                update_handle.set_models_at_edge(&split_handle.borrow(), id);
            }
        });
    }

    win.run();
    
}
