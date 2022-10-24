use slint::{Model, VecModel, Color};
use std::{rc::Rc, cell::RefCell};
slint::include_modules!();

/*
    So, I currently have something that stores its relative position on the screen (top, bottom, whatever) and resizes the things inside
        along one axis
    Is storing relative position necessary? Rather, if you're something inside that, you don't have one...
        function to get UniFrame top, breadth, and parent length, default to trait. Weak pointer to parent which returns such.
    Okay, so say you have a nested deal. Diff between UniFrame and UniSplit... Say that a UniFrame can contain a UniSplit.
        Then a UniFrame has a top and breadth function, which UniSplit points to?
        Then what is to_region? It returns a list of regions, I guess, which then get combined. Feels a bit wasteful... Rather: pass in a reference
            to a list, and add to it? Would have to be refcell. Why do appends have to be mutating? Ugh.
        Then what is the purpose of UniFrame? It tracks in terms of size, min_size, and offset. The generation is conducted by UniSplit, but only
            to get reference to orientation and size. If you move that to UniFrame, then the purpose of UniFrame...
                Well, I should be thinking in terms of structs and traits. UniSplit is what does the shit.
            No, it's recursive.
    UniFrame is the smallest unit. It generates a region. It stores the least shared data: the offset, size, and min-size.
        From parent UniFrame to child UniFrame, it communicates size, and passes on top and breadth.
        From child UniFrame to parent UniFrame, it communicates min-size, and passes on generated regions.
        If there is no child UniFrame, then there are no passed on generated regions.
            Then, the to_region is implemented by Splits. These check whether Frame is empty or not...
            No, to_region implemented by UniFrame. If empty, uses parent, if not, uses child? idk.
    What about the generated edges? These are produced relative to the Split. UniFrame only provides some data. So both to_region and to_edge
        should be from parent. The childsplit call returns a vector that can append on.
    
    How are top, breadth, and generated regions passed on? Well, for both, the split optionally produces it itself...
        Okay, so parentSplit owns frame owns childSplit. Weak links back the other way.
            Alt, co ownership of frame? Because there's data the other way in terms of min-size...
        childSplit has parentFrame weak and optional. If no frame, default top and breadth. Therefore, function called at childsplit level.
            Frame has permanent gettop and bottom deal. (stored in frame? No reason...) Maybe stores reference from parent? Sure, but no mutability
                with immutable reads, right? No can do.
        frame owns childSplit weak and optional. But it needs data from both parent and child.
            You could have a fall through. If no child, parent does toRegion. If with child, frame does it. But in the child toRegion calc
                it's gonna go through and access the parent top bottom a ton... Wouldn't it be better, then, to access it from the child?
        !! toRegion is provided with top and bottom values, which can be copied. No mutability issues.
            toRegion is at split level, so that there's no choosing between vec or regular return. Or rather that case is handled inside...
            frame passes on toRegion call as is, and passes on childSplit option as is. Just a communicator, when not used by split.
        Fuck... but right now, there's also passing in x position. Like, the data from the frame isn't even in use by the child, the data
            necessary has to be calculated by the parent split. As long as the parent associates a split with a frame, it doesn't matter about
            owner ship, about the split having access to frame data.

    So, parent owner of frame, frame owner of child. No need for weak refs the other way, because parent will generate needed size stuff
     when modding child.
        But what if a parent split changes size? rn it calls drag far and drag near on the frame. guessing that is optionally propagated to split.
            Then split has to have the same function? I don't like that duplicate... It does mean that resize and drag could be generalized.
            So, resize could be seen as a drag on the parent? No, bad idea, because there's no minimums in that situation. A drag could be rejected.
            So UniFrame handles the initial drag rejection. If it does actually happen, it calls resize on the underlying split: accurate to what
             needs to be modified. There is no need for a separate drag on the split because resize forcibly handles it. You just need a way to obtain
             min size recursively. Further drags in the child UniSplit also propagate recursively.
            Currently, this model stores size. The min_size is essentially the min_size of the window, going below just clips off. A change is made.
                The calculation is made. Then the model is updated from the new numbers. Except part of the model update is also calculation, because
                 of splits and offsets. You could go the direction of storing position for everyone, or storing percentages.
                If you store percentages, you'll still need a recursive get_min, but that's not something that's modified. Otherwise, a drag only
                 changes one value. Then, to get actual size, you have to calculate from the top. Except it's worth being about the call that calculation
                 from the bottom, because you won't know the size that you're trying to change to.

    fuck. Think about something different. So you have a recursive dealio. How do you link and update with the model, based on that?
        Update whole model basically recursively calls init_region and sets row data. That means that init_region has to consistently return in order.
        Resize calls from the top. Also calls recursively. uses same update model thing.
        Edge move affects two Uniframes. There has to be a way to update for only those two frames... Then there needs to be a frame specific update,
         which is also recursive. (could be the same "update" as that called from the top). Except one edge is excluded... that's a detail I don't
         care to leave out, it's just one edge out of two regions.
            Now, how can you make sure the right update is called? That means that a child frame has to be able to call its offset row position!
            Another number to provide...
            

    Okay, target. model. Two lists. Regions and edges. Describe positions and some sort of callback id.
    
    Structures:
        split: x, y, breadth, length, orientation, some kinda split_id.
            contains frame, edges, splits
        sizes:
            edge: offset from split beginning?, min-size from prev edge
            frame: size (offset from prev frame), min-size
                what is the point of also including offset? because then you can mod the start of one frame due to edge drag without going through all previous
    okay, no, this isn't what i want

    Resursion to target:
        any deletions or additions refresh entire model, cuz it's necessary. Well, how do you get callback id?
            split_id and region_id. region is relative to inside of split. Or... Single id related to row. It's just the index inside...
            run calc from bottom that gets id?
    
    in terms of commands...
    Remember, MVC. So, the view is already handled.
    We have two chained models. The list of regions and edges makes the view.
    The recursive structure makes the model.
    
    The controller has: drag edge, resize, add, delete that targets some callback in first typa model.
        What parts are these applied to?
        edge drag is applied to single edge, which affects adjacent frames.
            necessary data:
                needs to know what to impact, and what row to affect
                needs to know min-size and resulting size, or otherwise sometype of "stop further dragging" signal. Drag signal is px based.
                if px based:
                    Know adjacent frame, judge off that size.
                if % based:
                    Know container frame px.
                        container has to have some call to get_px that returns default or from parent. split link to parent split.
                    adjacent frame %s
                    min % from frame -> some conversion from px.
            Handling:
                px based:
                    split (list)
                        index into two regions.
                        split methods: drag(index, dist)
                        region methods: get min (recursive down). change sizes/offets.
                    linkage of frames, end excluded
                        given frame affects self and following
                        frame methods: access to offset. get min for both (recursive? meaning frame must link to contained frames). change sizes. drag_right_edge(dist)
                % based:
                    wait, idea. Only need container px, to det resulting % drag. Then just change it and call update? Update then clamps %...

        resize applied to single frame, propagates through children
            necessary data:
                gotta know child frames
                size of children, for relative resizing.
                mins
            handling: (det px based)
                list
                    already have child frame current sizes and mins. Calc new sizes, then set.
                    or, child methods: shrink that returns result. Shrink recurses to get actual shrinkage... Then, same for window or?
                        shrinkage stops at some point. That's fine, just stop doing shit... Except, on window expand.
                Window expand should be separate method. If shrinking, it applies shrink on given. However, if the window size is less than
                 parent frame, it takes no action. Only if larger does new width/height do anything... bet.
                linkage
                    hmm... maybe, if you have an edge, it has reference to all edges to its left? idk... don't like this method.
        ok, so we're going with the list?
            addition: addition at site.
            deletion: deletion from list. if only one remains -> collapse upward? So
                split[[frame, split[frame, split[frame, frame]]]]
                split[[frame, split[frame]]]
                split[[frame, frame]]
                Well, then shouldn't splits and frames just share the same shit...? What if the only optional was whether they had children?
                    I mean, a frame already stores its offset for convenience... other methods can be got from parents.
            sure. fine.
        
        how about indices? sol 1: unique ids with top level dict, all throughout accessible...
            on init: new frames/splits get random ids. A produced "region" or "edge" is given that id.
                on model update, returned list is pushed with ids. indices are irrelevant.
            on update all:
                returned list or dict with ids. For each, find matching in model, update that row.
            on callback:
                this is the hard part...
        reversible top level dict from id to object and back + id number counter. cool...
            This means a lotta shit has to be callable from frame level... Guess the point of the frame might just be as an id point, along
             with whatever info split needs from it. Okay, I think this is enough prep.
             Could have model track objects directly with row, and then model notify when changes. Gets rid of intermediate model... who cares.
              still gonna be by rows...
        Add... should still only update some. idk. A more specific model can allow for that in the notifs! so do that later, update all for now.

 */

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
