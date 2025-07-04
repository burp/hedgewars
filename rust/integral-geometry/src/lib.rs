use fpnum::{fp, integral_sqrt, FPNum, FPPoint};
use std::{
    cmp::{max, min},
    ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, RangeInclusive, Sub, SubAssign},
};

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub struct Point {
    pub x: i32,
    pub y: i32,
}

impl Point {
    pub const ZERO: Self = Self::new(0, 0);

    #[inline]
    pub const fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }

    #[inline]
    pub const fn diag(v: i32) -> Self {
        Self::new(v, v)
    }

    #[inline]
    pub const fn signum(self) -> Self {
        Self::new(self.x.signum(), self.y.signum())
    }

    #[inline]
    pub const fn abs(self) -> Self {
        Self::new(self.x.abs(), self.y.abs())
    }

    #[inline]
    pub const fn dot(self, other: Point) -> i32 {
        self.x * other.x + self.y * other.y
    }

    #[inline]
    pub fn max_norm(self) -> i32 {
        std::cmp::max(self.x.abs(), self.y.abs())
    }

    #[inline]
    pub fn integral_norm(self) -> u32 {
        let sqr = (self.x as u64).wrapping_pow(2) + (self.y as u64).wrapping_pow(2);
        integral_sqrt(sqr) as u32
    }

    #[inline]
    pub const fn transform(self, matrix: &[i32; 4]) -> Self {
        Point::new(
            matrix[0] * self.x + matrix[1] * self.y,
            matrix[2] * self.x + matrix[3] * self.y,
        )
    }

    #[inline]
    pub const fn rotate90(self) -> Self {
        Self::new(self.y, -self.x)
    }
    #[inline]
    pub const fn rotate180(self) -> Self {
        Self::new(-self.x, -self.y)
    }
    #[inline]
    pub const fn rotate270(self) -> Self {
        Self::new(-self.y, self.x)
    }

    #[inline]
    pub const fn cross(self, other: Point) -> i32 {
        self.dot(other.rotate90())
    }

    #[inline]
    pub fn clamp(self, rect: &Rect) -> Self {
        Self::new(rect.x_range().clamp(self.x), rect.y_range().clamp(self.y))
    }

    #[inline]
    pub const fn line_to(self, end: Point) -> Line {
        Line::new(self, end)
    }

    #[inline]
    pub const fn ray_with_dir(self, direction: Point) -> Ray {
        Ray::new(self, direction)
    }

    #[inline]
    pub const fn tangent_mul(self, x: i32) -> i32 {
        x * self.y / self.x
    }

    #[inline]
    pub const fn cotangent_mul(self, y: i32) -> i32 {
        y * self.x / self.y
    }

    #[inline]
    pub fn to_fppoint(self) -> FPPoint {
        FPPoint::new(self.x.into(), self.y.into())
    }

    #[inline]
    pub fn from_fppoint(p: &FPPoint) -> Self {
        Self::new(p.x().round() as i32, p.y().round() as i32)
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct Size {
    pub width: u32,
    pub height: u32,
}

impl Size {
    pub const EMPTY: Self = Self::square(0);

    #[inline]
    pub const fn new(width: u32, height: u32) -> Self {
        Self { width, height }
    }

    #[inline]
    pub const fn square(size: u32) -> Self {
        Self {
            width: size,
            height: size,
        }
    }

    #[inline]
    pub const fn area(&self) -> u32 {
        self.width * self.height
    }

    #[inline]
    pub const fn linear_index(&self, x: u32, y: u32) -> u32 {
        y * self.width + x
    }

    #[inline]
    pub const fn is_power_of_two(&self) -> bool {
        self.width.is_power_of_two() && self.height.is_power_of_two()
    }

    #[inline]
    pub const fn as_power_of_two(&self) -> Option<PotSize> {
        PotSize::new(self.width, self.height)
    }

    #[inline]
    pub const fn next_power_of_two(&self) -> PotSize {
        PotSize::new_impl(
            self.width.next_power_of_two(),
            self.height.next_power_of_two(),
        )
    }

    #[inline]
    pub const fn transpose(&self) -> Self {
        Self::new(self.height, self.width)
    }

    #[inline]
    pub fn to_square(&self) -> Self {
        Self::square(max(self.width, self.height))
    }

    #[inline]
    pub fn is_square(&self) -> bool {
        self.width == self.height
    }

    #[inline]
    pub const fn contains(&self, other: Self) -> bool {
        self.width >= other.width && self.height >= other.height
    }

    #[inline]
    pub fn join(&self, other: Self) -> Self {
        Self::new(max(self.width, other.width), max(self.height, other.height))
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct PotSize {
    size: Size,
}

impl PotSize {
    #[inline]
    const fn new_impl(width: u32, height: u32) -> Self {
        debug_assert!(width.is_power_of_two() && height.is_power_of_two());
        Self {
            size: Size::new(width, height),
        }
    }

    #[inline]
    pub const fn new(width: u32, height: u32) -> Option<Self> {
        if width.is_power_of_two() && height.is_power_of_two() {
            Some(Self::new_impl(width, height))
        } else {
            None
        }
    }

    pub const fn size(&self) -> Size {
        self.size
    }

    pub const fn width(&self) -> u32 {
        self.size.width
    }

    pub const fn height(&self) -> u32 {
        self.size.height
    }

    #[inline]
    pub const fn square(size: u32) -> Option<Self> {
        if size.is_power_of_two() {
            Some(Self::new_impl(size, size))
        } else {
            None
        }
    }

    #[inline]
    pub const fn area(&self) -> u32 {
        self.size.area()
    }

    #[inline]
    pub const fn linear_index(&self, x: u32, y: u32) -> u32 {
        self.size.linear_index(x, y)
    }

    #[inline]
    pub const fn transpose(&self) -> Self {
        Self::new_impl(self.height(), self.width())
    }

    #[inline]
    pub fn to_square(&self) -> Self {
        let size = max(self.width(), self.height());
        Self::new_impl(size, size)
    }

    #[inline]
    pub const fn to_mask(&self) -> SizeMask {
        SizeMask::new(*self)
    }

    pub const fn to_grid_index(&self) -> GridIndex {
        GridIndex::new(*self)
    }

    #[inline]
    pub const fn contains(&self, other: Self) -> bool {
        self.size.contains(other.size)
    }

    #[inline]
    pub fn join(&self, other: Self) -> Self {
        Self::new_impl(
            max(self.width(), other.width()),
            max(self.height(), other.height()),
        )
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct SizeMask {
    size: Size,
}

impl SizeMask {
    #[inline]
    pub const fn new(size: PotSize) -> Self {
        Self {
            size: Size::new(!(size.width() - 1), !(size.height() - 1)),
        }
    }

    #[inline]
    pub fn contains_x<T: Into<u32>>(&self, x: T) -> bool {
        (self.size.width & x.into()) == 0
    }

    #[inline]
    pub fn contains_y<T: Into<u32>>(&self, y: T) -> bool {
        (self.size.height & y.into()) == 0
    }

    #[inline]
    pub fn contains(&self, point: Point) -> bool {
        self.contains_x(point.x as u32) && self.contains_y(point.y as u32)
    }

    #[inline]
    pub const fn to_size(&self) -> PotSize {
        PotSize::new_impl(!self.size.width + 1, !self.size.height + 1)
    }
}

pub struct GridIndex {
    shift: Point,
}

impl GridIndex {
    pub const fn new(size: PotSize) -> Self {
        let shift = Point::new(
            size.width().trailing_zeros() as i32,
            size.height().trailing_zeros() as i32,
        );
        Self { shift }
    }

    pub const fn map(&self, position: Point) -> Point {
        Point::new(position.x >> self.shift.x, position.y >> self.shift.y)
    }
}

macro_rules! bin_op_impl {
    ($op: ty, $name: tt) => {
        impl $op for Point {
            type Output = Self;

            #[inline]
            fn $name(self, rhs: Self) -> Self::Output {
                Self::new(self.x.$name(rhs.x), self.y.$name(rhs.y))
            }
        }
    };
}

macro_rules! scalar_bin_op_impl {
    ($($op: tt)::+, $name: tt) => {
        impl $($op)::+<i32> for Point {
            type Output = Self;

            #[inline]
            fn $name(self, rhs: i32) -> Self::Output {
                Self::new(self.x.$name(rhs), self.y.$name(rhs))
            }
        }
    };
}

macro_rules! bin_assign_op_impl {
    ($op: ty, $name: tt) => {
        impl $op for Point {
            #[inline]
            fn $name(&mut self, rhs: Self) {
                self.x.$name(rhs.x);
                self.y.$name(rhs.y);
            }
        }
    };
}

macro_rules! fp_scalar_bin_op_impl {
    ($($op: tt)::+, $name: tt) => {
        impl $($op)::+<FPNum> for Point {
            type Output = FPPoint;

            #[inline]
            fn $name(self, rhs: FPNum) -> Self::Output {
                FPPoint::new(rhs.$name(self.x), rhs.$name(self.y))
            }
        }
    };
}

macro_rules! left_fp_scalar_bin_op_impl {
    ($($op: tt)::+, $name: tt) => {
        impl $($op)::+<Point> for FPNum {
            type Output = FPPoint;

            #[inline]
            fn $name(self, rhs: Point) -> Self::Output {
                FPPoint::new(self.$name(rhs.x), self.$name(rhs.y))
            }
        }
    };
}

bin_op_impl!(Add, add);
bin_op_impl!(Sub, sub);
bin_op_impl!(Mul, mul);
bin_op_impl!(Div, div);
scalar_bin_op_impl!(Mul, mul);
scalar_bin_op_impl!(Div, div);
fp_scalar_bin_op_impl!(Mul, mul);
fp_scalar_bin_op_impl!(Div, div);
left_fp_scalar_bin_op_impl!(Mul, mul);
left_fp_scalar_bin_op_impl!(Div, div);
bin_assign_op_impl!(AddAssign, add_assign);
bin_assign_op_impl!(SubAssign, sub_assign);
bin_assign_op_impl!(MulAssign, mul_assign);
bin_assign_op_impl!(DivAssign, div_assign);

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct Rect {
    top_left: Point,
    bottom_right: Point,
}

impl Rect {
    pub const EMPTY: Self = Self {
        top_left: Point::ZERO,
        bottom_right: Point::diag(-1),
    };

    #[inline]
    pub const fn new(top_left: Point, bottom_right: Point) -> Self {
        debug_assert!(top_left.x <= bottom_right.x + 1);
        debug_assert!(top_left.y <= bottom_right.y + 1);
        Self {
            top_left,
            bottom_right,
        }
    }

    pub const fn from_box(left: i32, right: i32, top: i32, bottom: i32) -> Self {
        Self::new(Point::new(left, top), Point::new(right, bottom))
    }

    pub fn from_size(top_left: Point, size: Size) -> Self {
        Self::new(
            top_left,
            top_left + Point::new(size.width as i32 - 1, size.height as i32 - 1),
        )
    }

    pub fn from_size_coords(x: i32, y: i32, width: u32, height: u32) -> Self {
        Self::from_size(Point::new(x, y), Size::new(width, height))
    }

    pub fn at_origin(size: Size) -> Self {
        Self::from_size(Point::ZERO, size)
    }

    #[inline]
    pub const fn width(&self) -> u32 {
        (self.right() - self.left() + 1) as u32
    }

    #[inline]
    pub const fn height(&self) -> u32 {
        (self.bottom() - self.top() + 1) as u32
    }

    #[inline]
    pub const fn size(&self) -> Size {
        Size::new(self.width(), self.height())
    }

    #[inline]
    pub const fn area(&self) -> u32 {
        self.size().area()
    }

    #[inline]
    pub const fn left(&self) -> i32 {
        self.top_left().x
    }

    #[inline]
    pub const fn top(&self) -> i32 {
        self.top_left().y
    }

    #[inline]
    pub const fn right(&self) -> i32 {
        self.bottom_right().x
    }

    #[inline]
    pub const fn bottom(&self) -> i32 {
        self.bottom_right().y
    }

    #[inline]
    pub const fn top_left(&self) -> Point {
        self.top_left
    }

    #[inline]
    pub const fn bottom_right(&self) -> Point {
        self.bottom_right
    }

    #[inline]
    pub fn center(&self) -> Point {
        (self.top_left() + self.bottom_right()) / 2
    }

    #[inline]
    pub fn with_margin(&self, margin: i32) -> Self {
        let offset = Point::new(
            min(margin, self.width() as i32 / 2),
            min(margin, self.height() as i32 / 2),
        );
        Self::new(self.top_left() + offset, self.bottom_right() - offset)
    }

    #[inline]
    pub const fn x_range(&self) -> RangeInclusive<i32> {
        self.left()..=self.right()
    }

    #[inline]
    pub const fn y_range(&self) -> RangeInclusive<i32> {
        self.top()..=self.bottom()
    }

    #[inline]
    pub fn contains(&self, point: Point) -> bool {
        self.x_range().contains(&point.x) && self.y_range().contains(&point.y)
    }

    #[inline]
    pub const fn contains_inside(&self, point: Point) -> bool {
        point.x > self.left()
            && point.x < self.right()
            && point.y > self.top()
            && point.y < self.bottom()
    }

    #[inline]
    pub fn contains_rect(&self, other: &Self) -> bool {
        self.contains(other.top_left()) && self.contains(other.bottom_right())
    }

    #[inline]
    pub const fn intersects(&self, other: &Rect) -> bool {
        self.left() <= other.right()
            && self.right() >= other.left()
            && self.top() <= other.bottom()
            && self.bottom() >= other.top()
    }

    #[inline]
    pub const fn split_at(&self, point: Point) -> [Rect; 4] {
        debug_assert!(self.contains_inside(point));
        [
            Self::from_box(self.left(), point.x, self.top(), point.y),
            Self::from_box(point.x, self.right(), self.top(), point.y),
            Self::from_box(point.x, self.right(), point.y, self.bottom()),
            Self::from_box(self.left(), point.x, point.y, self.bottom()),
        ]
    }

    #[inline]
    pub const fn with_margins(&self, left: i32, right: i32, top: i32, bottom: i32) -> Self {
        Self::from_box(
            self.left() - left,
            self.right() + right,
            self.top() - top,
            self.bottom() + bottom,
        )
    }

    #[inline]
    pub fn quotient(self, x: u32, y: u32) -> Point {
        self.top_left() + Point::new((x % self.width()) as i32, (y % self.height()) as i32)
    }
}

trait RangeClamp<T> {
    fn clamp(&self, value: T) -> T;
}

impl<T: Ord + Copy> RangeClamp<T> for RangeInclusive<T> {
    fn clamp(&self, value: T) -> T {
        if value < *self.start() {
            *self.start()
        } else if value > *self.end() {
            *self.end()
        } else {
            value
        }
    }
}

pub struct Polygon {
    vertices: Vec<Point>,
}

impl Polygon {
    pub fn new(vertices: &[Point]) -> Self {
        let mut v = Vec::with_capacity(vertices.len() + 1);
        v.extend_from_slice(vertices);
        if !v.is_empty() {
            let start = v[0];
            v.push(start);
        }
        Self { vertices: v }
    }

    pub fn edges_count(&self) -> usize {
        self.vertices.len().saturating_sub(1)
    }

    pub fn get_edge(&self, index: usize) -> Line {
        Line::new(self.vertices[index], self.vertices[index + 1])
    }

    pub fn split_edge(&mut self, edge_index: usize, vertex: Point) {
        self.vertices.insert(edge_index + 1, vertex);
    }

    pub fn iter(&self) -> impl Iterator<Item = &Point> {
        (&self.vertices[..self.edges_count()]).iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Point> {
        let edges_count = self.edges_count();
        let start = self.vertices.as_mut_ptr();
        let end = unsafe { start.add(edges_count) };
        PolygonPointsIteratorMut {
            source: self,
            start,
            end,
        }
    }

    fn force_close(&mut self) {
        if !self.vertices.is_empty() {
            let edges_count = self.edges_count();
            self.vertices[edges_count] = self.vertices[0];
        }
    }

    pub fn iter_edges<'a>(&'a self) -> impl Iterator<Item = Line> + 'a {
        (&self.vertices[0..self.edges_count()])
            .iter()
            .zip(&self.vertices[1..])
            .map(|(s, e)| Line::new(*s, *e))
    }

    pub fn bezierize(&mut self, segments_number: u32) {
        fn calc_point(p1: Point, p2: Point, p3: Point) -> FPPoint {
            let diff13 = (p1 - p3).to_fppoint();
            let diff13_norm = diff13.distance();

            if diff13_norm.is_zero() {
                diff13
            } else {
                let diff12_norm = (p1 - p2).to_fppoint().distance();
                let diff23_norm = (p2 - p3).to_fppoint().distance();
                let min_distance = min(diff13_norm, min(diff12_norm, diff23_norm));

                diff13 * min_distance / diff13_norm / 3
            }
        }

        if self.vertices.len() < 4 {
            return;
        }

        let delta = fp!(1 / segments_number);
        let mut bezierized_vertices = Vec::new();
        let mut pi = 0;
        let mut i = 1;
        let mut ni = 2;
        let mut right_point = calc_point(self.vertices[pi], self.vertices[i], self.vertices[ni]);
        let mut left_point;

        pi += 1;
        while pi != 0 {
            pi = i;
            i = ni;
            ni += 1;
            if ni >= self.vertices.len() {
                ni = 0;
            }

            left_point = right_point;
            right_point = calc_point(self.vertices[pi], self.vertices[i], self.vertices[ni]);

            bezierized_vertices.extend(BezierCurveSegments::new(
                Line::new(self.vertices[pi], self.vertices[i]),
                left_point,
                -right_point,
                delta,
            ));
        }

        self.vertices = bezierized_vertices;
    }
}

struct PolygonPointsIteratorMut<'a> {
    source: &'a mut Polygon,
    start: *mut Point,
    end: *mut Point,
}

impl<'a> Iterator for PolygonPointsIteratorMut<'a> {
    type Item = &'a mut Point;

    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        if self.start == self.end {
            None
        } else {
            unsafe {
                let result = &mut *self.start;
                self.start = self.start.add(1);
                Some(result)
            }
        }
    }
}

impl<'a> Drop for PolygonPointsIteratorMut<'a> {
    fn drop(&mut self) {
        self.source.force_close();
    }
}

impl From<Vec<Point>> for Polygon {
    fn from(mut v: Vec<Point>) -> Self {
        if !v.is_empty() && v[0] != v[v.len() - 1] {
            let start = v[0];
            v.push(start)
        }
        Self { vertices: v }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct Ray {
    pub start: Point,
    pub direction: Point,
}

impl Ray {
    #[inline]
    pub const fn new(start: Point, direction: Point) -> Ray {
        Self { start, direction }
    }

    #[inline]
    pub const fn tangent_mul(&self, x: i32) -> i32 {
        self.direction.tangent_mul(x)
    }

    #[inline]
    pub const fn cotangent_mul(&self, y: i32) -> i32 {
        self.direction.cotangent_mul(y)
    }

    #[inline]
    pub fn orientation(&self, point: Point) -> i32 {
        (point - self.start).cross(self.direction).signum()
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct Line {
    pub start: Point,
    pub end: Point,
}

impl Line {
    pub const ZERO: Self = Self::new(Point::ZERO, Point::ZERO);

    #[inline]
    pub const fn new(start: Point, end: Point) -> Self {
        Self { start, end }
    }

    #[inline]
    pub fn center(&self) -> Point {
        (self.start + self.end) / 2
    }

    #[inline]
    pub fn scaled_direction(&self) -> Point {
        self.end - self.start
    }

    #[inline]
    pub fn scaled_normal(&self) -> Point {
        self.scaled_direction().rotate90()
    }

    #[inline]
    pub fn to_ray(&self) -> Ray {
        Ray::new(self.start, self.scaled_direction())
    }
}

impl IntoIterator for Line {
    type Item = Point;
    type IntoIter = LinePoints;

    fn into_iter(self) -> Self::IntoIter {
        LinePoints::new(self)
    }
}

pub struct LinePoints {
    accumulator: Point,
    direction: Point,
    sign: Point,
    current: Point,
    total_steps: i32,
    step: i32,
}

impl LinePoints {
    pub fn new(line: Line) -> Self {
        let dir = line.end - line.start;

        Self {
            accumulator: Point::ZERO,
            direction: dir.abs(),
            sign: dir.signum(),
            current: line.start,
            total_steps: dir.max_norm(),
            step: 0,
        }
    }
}

impl Iterator for LinePoints {
    type Item = Point;

    fn next(&mut self) -> Option<Self::Item> {
        if self.step <= self.total_steps {
            self.accumulator += self.direction;

            if self.accumulator.x > self.total_steps {
                self.accumulator.x -= self.total_steps;
                self.current.x += self.sign.x;
            }
            if self.accumulator.y > self.total_steps {
                self.accumulator.y -= self.total_steps;
                self.current.y += self.sign.y;
            }

            self.step += 1;

            Some(self.current)
        } else {
            None
        }
    }
}

pub struct ArcPoints {
    point: Point,
    step: i32,
}

impl ArcPoints {
    pub const fn new(radius: i32) -> Self {
        Self {
            point: Point::new(0, radius),
            step: 3 - 2 * radius,
        }
    }
}

impl Iterator for ArcPoints {
    type Item = Point;

    fn next(&mut self) -> Option<Self::Item> {
        if self.point.x < self.point.y {
            let result = self.point;

            if self.step < 0 {
                self.step += self.point.x * 4 + 6;
            } else {
                self.step += (self.point.x - self.point.y) * 4 + 10;
                self.point.y -= 1;
            }

            self.point.x += 1;

            Some(result)
        } else if self.point.x == self.point.y {
            self.point.x += 1;

            Some(self.point)
        } else {
            None
        }
    }
}

pub struct EquidistantPoints {
    vector: Vec<Point>,
}

impl EquidistantPoints {
    pub fn new(vector: Point) -> Self {
        Self {
            vector: if vector.x == vector.y {
                vec![
                    Point::new(vector.x, vector.x),
                    Point::new(vector.x, -vector.x),
                    Point::new(-vector.x, -vector.x),
                    Point::new(-vector.x, vector.x),
                ]
            } else {
                vec![
                    Point::new(vector.x, vector.y),
                    Point::new(vector.x, -vector.y),
                    Point::new(-vector.x, -vector.y),
                    Point::new(-vector.x, vector.y),
                    Point::new(vector.y, vector.x),
                    Point::new(vector.y, -vector.x),
                    Point::new(-vector.y, -vector.x),
                    Point::new(-vector.y, vector.x),
                ]
            },
        }
    }
}

impl IntoIterator for EquidistantPoints {
    type Item = Point;
    type IntoIter = std::vec::IntoIter<Point>;

    fn into_iter(self) -> Self::IntoIter {
        self.vector.into_iter()
    }
}

pub struct BezierCurveSegments {
    segment: Line,
    control_point1: FPPoint,
    control_point2: FPPoint,
    offset: FPNum,
    max_offset: FPNum,
    delta: FPNum,
    have_finished: bool,
}

impl BezierCurveSegments {
    pub fn new(segment: Line, p1: FPPoint, p2: FPPoint, delta: FPNum) -> Self {
        Self {
            segment,
            control_point1: segment.start.to_fppoint() - p1,
            control_point2: segment.end.to_fppoint() - p2,
            offset: fp!(0),
            max_offset: fp!(4095 / 4096),
            delta,
            have_finished: false,
        }
    }
}

impl Iterator for BezierCurveSegments {
    type Item = Point;

    fn next(&mut self) -> Option<Self::Item> {
        if self.offset < self.max_offset {
            let offset_sq = self.offset * self.offset;
            let offset_cub = offset_sq * self.offset;

            let r1 = fp!(1) - self.offset * 3 + offset_sq * 3 - offset_cub;
            let r2 = self.offset * 3 - offset_sq * 6 + offset_cub * 3;
            let r3 = offset_sq * 3 - offset_cub * 3;

            let p = r1 * self.segment.start
                + r2 * self.control_point1
                + r3 * self.control_point2
                + offset_cub * self.segment.end;

            self.offset += self.delta;

            Some(Point::from_fppoint(&p))
        } else if !self.have_finished {
            self.have_finished = true;

            Some(self.segment.end)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn get_points(coords: &[(i32, i32)]) -> Vec<Point> {
        coords.iter().map(|(x, y)| Point::new(*x, *y)).collect()
    }

    #[test]
    fn line_basic() {
        let line: Vec<Point> = Line::new(Point::new(0, 0), Point::new(3, 3))
            .into_iter()
            .collect();
        let v = get_points(&[(0, 0), (1, 1), (2, 2), (3, 3)]);

        assert_eq!(line, v);
    }

    #[test]
    fn line_skewed() {
        let line: Vec<Point> = Line::new(Point::new(0, 0), Point::new(5, -7))
            .into_iter()
            .collect();
        let v = get_points(&[
            (0, 0),
            (1, -1),
            (2, -2),
            (2, -3),
            (3, -4),
            (4, -5),
            (4, -6),
            (5, -7),
        ]);

        assert_eq!(line, v);
    }

    #[test]
    fn equidistant_full() {
        let n: Vec<Point> = EquidistantPoints::new(Point::new(1, 3))
            .into_iter()
            .collect();
        let v = get_points(&[
            (1, 3),
            (1, -3),
            (-1, -3),
            (-1, 3),
            (3, 1),
            (3, -1),
            (-3, -1),
            (-3, 1),
        ]);

        assert_eq!(n, v);
    }

    #[test]
    fn equidistant_half() {
        let n: Vec<Point> = EquidistantPoints::new(Point::new(2, 2))
            .into_iter()
            .collect();
        let v = get_points(&[(2, 2), (2, -2), (-2, -2), (-2, 2)]);

        assert_eq!(n, v);
    }

    #[test]
    fn line() {
        let l = Line::new(Point::new(1, 1), Point::new(5, 6));

        assert_eq!(l.center(), Point::new(3, 3));
    }

    #[test]
    fn rect() {
        let r = Rect::from_box(10, 100, 0, 70);

        assert!(r.contains_inside(Point::new(99, 69)));
        assert!(!r.contains_inside(Point::new(100, 70)));

        assert_eq!(r.top_left(), Point::new(10, 0));
        assert_eq!(r.with_margin(12), Rect::from_box(22, 88, 12, 58));
    }

    #[test]
    fn fit() {
        let r = Rect::from_box(10, 100, 0, 70);

        assert_eq!(Point::new(0, -10).clamp(&r), Point::new(10, 0));
        assert_eq!(Point::new(1000, 1000).clamp(&r), Point::new(100, 70));
    }
}
