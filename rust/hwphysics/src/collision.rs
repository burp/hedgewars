use std::ops::RangeInclusive;

use crate::{common::GearId, data::GearDataManager, grid::Grid};

use fpnum::*;
use integral_geometry::{Point, PotSize};
use land2d::Land2D;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct CircleBounds {
    pub center: FPPoint,
    pub radius: FPNum,
}

impl CircleBounds {
    pub fn intersects(&self, other: &CircleBounds) -> bool {
        (other.center - self.center).is_in_range(self.radius + other.radius)
    }

    pub fn rows(&self) -> impl Iterator<Item = (usize, RangeInclusive<usize>)> {
        let radius = self.radius.abs_round() as usize;
        let center = Point::from_fppoint(&self.center);
        (center.y as usize - radius..=center.y as usize + radius)
            .map(move |row| (row, center.x as usize - radius..=center.x as usize + radius))
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct CollisionData {
    pub bounds: CircleBounds,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct ContactData {
    pub elasticity: FPNum,
    pub friction: FPNum,
}

struct EnabledCollisionsCollection {
    gear_ids: Vec<GearId>,
    collisions: Vec<CollisionData>,
}

impl EnabledCollisionsCollection {
    fn new() -> Self {
        Self {
            gear_ids: Vec::new(),
            collisions: Vec::new(),
        }
    }

    fn push(&mut self, gear_id: GearId, collision: CollisionData) {
        self.gear_ids.push(gear_id);
        self.collisions.push(collision);
    }

    fn iter(&self) -> impl Iterator<Item = (GearId, &CollisionData)> {
        self.gear_ids.iter().cloned().zip(self.collisions.iter())
    }
}

pub struct CollisionProcessor {
    grid: Grid,
    enabled_collisions: EnabledCollisionsCollection,

    detected_collisions: DetectedCollisions,
}

pub struct DetectedCollisions {
    pub pairs: Vec<(GearId, Option<GearId>)>,
    pub positions: Vec<Point>,
}

impl DetectedCollisions {
    pub fn new(capacity: usize) -> Self {
        Self {
            pairs: Vec::with_capacity(capacity),
            positions: Vec::with_capacity(capacity),
        }
    }

    pub fn push(
        &mut self,
        contact_gear_id1: GearId,
        contact_gear_id2: Option<GearId>,
        position: &FPPoint,
    ) {
        self.pairs.push((contact_gear_id1, contact_gear_id2));
        self.positions.push(Point::from_fppoint(&position));
    }

    pub fn clear(&mut self) {
        self.pairs.clear();
        self.positions.clear()
    }
}

impl CollisionProcessor {
    pub fn register_components(data: &mut GearDataManager) {
        data.register::<CollisionData>();
        data.register::<ContactData>();
    }

    pub fn new(size: PotSize) -> Self {
        Self {
            grid: Grid::new(size),
            enabled_collisions: EnabledCollisionsCollection::new(),
            detected_collisions: DetectedCollisions::new(0),
        }
    }

    pub fn add(&mut self, gear_id: GearId, gear_data: CollisionData) {
        self.grid.insert(gear_id, &gear_data.bounds);
    }

    pub fn remove(&mut self, gear_id: GearId) {
        self.grid.remove(gear_id, None);
    }

    pub fn get(&mut self, gear_id: GearId) -> Option<CollisionData> {
        None
    }

    pub fn process(
        &mut self,
        land: &Land2D<u32>,
        updates: &crate::physics::PositionUpdates,
    ) -> &DetectedCollisions {
        self.detected_collisions.clear();
        self.grid.check_collisions(&mut self.detected_collisions);

        for (gear_id, collision) in self.enabled_collisions.iter() {
            if collision
                .bounds
                .rows()
                .any(|(y, r)| (&land[y][r]).iter().any(|v| *v != 0))
            {
                self.detected_collisions
                    .push(gear_id, None, &collision.bounds.center)
            }
        }

        &self.detected_collisions
    }
}
