use rand::Rng;

pub mod maze;
pub mod outline_template_based;
pub mod wavefront_collapse;

#[derive(Clone, Copy)]
pub struct LandGenerationParameters<T> {
    zero: T,
    basic: T,
    distance_divisor: u32,
    skip_distort: bool,
    skip_bezier: bool,
}

impl<T: Copy + PartialEq + Default> LandGenerationParameters<T> {
    pub fn new(
        zero: T,
        basic: T,
        distance_divisor: u32,
        skip_distort: bool,
        skip_bezier: bool,
    ) -> Self {
        Self {
            zero,
            basic,
            distance_divisor,
            skip_distort,
            skip_bezier,
        }
    }

    pub fn zero(&self) -> T {
        self.zero
    }

    pub fn basic(&self) -> T {
        self.basic
    }
}

pub trait LandGenerator {
    fn generate_land<T: Copy + PartialEq + Default>(
        &self,
        parameters: &LandGenerationParameters<T>,
        prng: &mut impl Rng,
    ) -> land2d::Land2D<T>;
}
