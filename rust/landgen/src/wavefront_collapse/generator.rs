use super::tile_image::{Edge, EdgeSet, MatchSide, TileImage};
use super::wavefront_collapse::{CollapseRule, Tile, WavefrontCollapse};
use crate::{LandGenerationParameters, LandGenerator};
use integral_geometry::Size;
use png::Decoder;
use rand::Rng;
use std::collections::HashSet;
use std::fs::File;
use std::io::{BufReader, Result};
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct EdgeDescription {
    pub name: String,
    pub reversed: Option<bool>,
    pub symmetrical: Option<bool>,
    pub hard_match: Option<bool>,
}

#[derive(Debug, Clone)]
pub struct EdgesDescription {
    pub top: EdgeDescription,
    pub right: EdgeDescription,
    pub bottom: EdgeDescription,
    pub left: EdgeDescription,
}

#[derive(Debug, Clone)]
pub struct TileDescription {
    pub name: String,
    pub weight: u8,
    pub edges: EdgesDescription,
    pub anti_match: Option<[u64; 4]>,
    pub is_negative: Option<bool>,
    pub can_flip: Option<bool>,
    pub can_mirror: Option<bool>,
    pub can_rotate90: Option<bool>,
    pub can_rotate180: Option<bool>,
    pub can_rotate270: Option<bool>,
}

#[derive(Debug, Clone)]
pub struct ComplexEdgeDescription {
    pub begin: Option<EdgeDescription>,
    pub fill: Option<EdgeDescription>,
    pub end: Option<EdgeDescription>,
}

#[derive(Debug, Clone)]
pub struct NonStrictComplexEdgesDescription {
    pub top: Option<ComplexEdgeDescription>,
    pub right: Option<ComplexEdgeDescription>,
    pub bottom: Option<ComplexEdgeDescription>,
    pub left: Option<ComplexEdgeDescription>,
}

#[derive(Debug, Clone)]
pub struct TemplateDescription {
    pub size: Size,
    pub tiles: Vec<TileDescription>,
    pub edges: NonStrictComplexEdgesDescription,
    pub can_invert: bool,
    pub is_negative: bool,
    pub wrap: bool,
}

pub struct WavefrontCollapseLandGenerator {
    pub template: TemplateDescription,
    data_path: PathBuf,
}

impl WavefrontCollapseLandGenerator {
    pub fn new(template: TemplateDescription, data_path: &Path) -> Self {
        Self {
            template,
            data_path: data_path.to_owned(),
        }
    }

    fn load_image_tiles<T: Copy + PartialEq + Default>(
        &self,
        parameters: &LandGenerationParameters<T>,
        tile_description: &TileDescription,
    ) -> Result<Vec<TileImage<T, String>>> {
        let mut result = Vec::new();

        let file = File::open(
            self.data_path
                .join("Tiles")
                .join(&tile_description.name)
                .as_path(),
        )?;
        let decoder = Decoder::new(BufReader::new(file));
        let mut reader = decoder.read_info()?;

        let info = reader.info();
        let mut tiles_image =
            vec2d::Vec2D::new(&Size::new(info.width, info.height), parameters.zero);

        let mut buf = vec![0; reader.output_buffer_size()];
        let info = reader.next_frame(&mut buf)?;
        let bytes = &buf[..info.buffer_size()];

        let mut tiles_image_pixels = tiles_image.as_mut_slice().iter_mut();

        let (zero, basic) = if tile_description.is_negative.unwrap_or_default() {
            (parameters.basic(), parameters.zero())
        } else {
            (parameters.zero(), parameters.basic())
        };

        match info.color_type.samples() {
            1 => {
                for line in bytes.chunks_exact(info.line_size) {
                    for value in line.iter() {
                        *tiles_image_pixels
                            .next()
                            .expect("vec2d size matching image dimensions") =
                            if *value == 0 { zero } else { basic };
                    }
                }
            }
            a => {
                for line in bytes.chunks_exact(info.line_size) {
                    for value in line.chunks_exact(a) {
                        *tiles_image_pixels
                            .next()
                            .expect("vec2d size matching image dimensions") =
                            if value[0] == 0u8 { zero } else { basic };
                    }
                }
            }
        }

        let edge_set: EdgeSet<String> = EdgeSet::new([
            (&tile_description.edges.top).into(),
            (&tile_description.edges.right).into(),
            (&tile_description.edges.bottom).into(),
            (&tile_description.edges.left).into(),
        ]);

        let tile = TileImage::<T, String>::new(
            tiles_image,
            tile_description.weight,
            edge_set,
            tile_description.anti_match.unwrap_or_default(),
        );

        result.push(tile.clone());

        if tile_description.can_flip.unwrap_or_default() {
            result.push(tile.flipped());
        }
        if tile_description.can_mirror.unwrap_or_default() {
            result.push(tile.mirrored());
        }
        if tile_description.can_flip.unwrap_or_default()
            && tile_description.can_mirror.unwrap_or_default()
        {
            result.push(tile.mirrored().flipped());
        }

        if tile_description.can_rotate90.unwrap_or_default() {
            result.push(tile.rotated90());
        }
        if tile_description.can_rotate180.unwrap_or_default() {
            result.push(tile.rotated180());
        }
        if tile_description.can_rotate270.unwrap_or_default() {
            result.push(tile.rotated270());
        }

        Ok(result)
    }

    pub fn load_template<T: Copy + PartialEq + Default>(
        &self,
        parameters: &LandGenerationParameters<T>,
    ) -> Vec<TileImage<T, String>> {
        let mut result = Vec::new();

        for tile_description in self.template.tiles.iter() {
            if let Ok(mut tiles) = self.load_image_tiles(parameters, tile_description) {
                result.append(&mut tiles);
            } else {
                eprintln!("Failed to load a tile!");
            }
        }

        result
    }

    pub fn build_rules<T: Copy + PartialEq + Default>(
        &self,
        tiles: &[TileImage<T, String>],
        probability_distribution_factor: i32,
    ) -> Vec<CollapseRule> {
        let [grid_top_edge, grid_right_edge, grid_bottom_edge, grid_left_edge]: [Option<
            [Option<Edge<String>>; 3],
        >; 4] = [
            self.template.edges.top.as_ref(),
            self.template.edges.right.as_ref(),
            self.template.edges.bottom.as_ref(),
            self.template.edges.left.as_ref(),
        ]
        .map(|opt| opt.map(|d| [&d.begin, &d.fill, &d.end].map(|e| e.as_ref().map(Into::into))));

        let mut rules = Vec::<CollapseRule>::new();

        for (i, tile) in tiles.iter().enumerate() {
            let mut top = HashSet::new();
            let mut right = HashSet::new();
            let mut bottom = HashSet::new();
            let mut left = HashSet::new();

            let iteration = [
                (&grid_top_edge, tile.edge_set().top(), &mut top),
                (&grid_right_edge, tile.edge_set().right(), &mut right),
                (&grid_bottom_edge, tile.edge_set().bottom(), &mut bottom),
                (&grid_left_edge, tile.edge_set().left(), &mut left),
            ];

            // compatibility with grid edges
            for (edge, tile_edge, set) in iteration {
                if !tile_edge.hard_match() {
                    set.insert(Tile::Empty);
                }

                for (is_compatible, tile) in edge
                    .as_ref()
                    .map(|e| {
                        e.clone().map(|ed| {
                            ed.as_ref()
                                .map(|e| e.is_compatible(tile_edge))
                                .unwrap_or(true)
                        })
                    })
                    .unwrap_or([true, true, true])
                    .into_iter()
                    .zip([Tile::OutsideBegin, Tile::OutsideFill, Tile::OutsideEnd].into_iter())
                {
                    if is_compatible {
                        set.insert(tile);
                    }
                }
            }

            // compatibility with itself
            if tile.is_compatible(&tile, MatchSide::OnLeft) {
                left.insert(Tile::Numbered(i));
                right.insert(Tile::Numbered(i));
            }

            if tile.is_compatible(&tile, MatchSide::OnTop) {
                top.insert(Tile::Numbered(i));
                bottom.insert(Tile::Numbered(i));
            }

            // compatibility with previously defined tiles
            for p in 0..i {
                // Check left edge
                if tiles[p].is_compatible(&tile, MatchSide::OnLeft) {
                    rules[p].left.insert(Tile::Numbered(i));
                    right.insert(Tile::Numbered(p));
                }

                // Check right edge
                if tiles[p].is_compatible(&tile, MatchSide::OnRight) {
                    rules[p].right.insert(Tile::Numbered(i));
                    left.insert(Tile::Numbered(p));
                }

                // Check top edge
                if tiles[p].is_compatible(&tile, MatchSide::OnTop) {
                    rules[p].top.insert(Tile::Numbered(i));
                    bottom.insert(Tile::Numbered(p));
                }

                // Check bottom edge
                if tiles[p].is_compatible(&tile, MatchSide::OnBottom) {
                    rules[p].bottom.insert(Tile::Numbered(i));
                    top.insert(Tile::Numbered(p));
                }
            }

            let weight = (probability_distribution_factor * 2 * i as i32 / (tiles.len() - 1) as i32
                + 100
                - probability_distribution_factor) as u32;

            rules.push(CollapseRule {
                weight: weight * tile.weight as u32,
                tile: Tile::Numbered(i),
                top,
                right,
                bottom,
                left,
            });
        }

        rules
    }
}

impl LandGenerator for WavefrontCollapseLandGenerator {
    fn generate_land<T: Copy + PartialEq + Default>(
        &self,
        parameters: &LandGenerationParameters<T>,
        random_numbers: &mut impl Rng,
    ) -> land2d::Land2D<T> {
        assert!(parameters.distance_divisor >= 1);
        assert!(parameters.distance_divisor <= 25);

        let tiles = self.load_template(parameters);
        let distribution_factor = (parameters.distance_divisor - 1) as i32 * 8 - 96;
        let rules = self.build_rules(&tiles, distribution_factor);

        let mut wfc = WavefrontCollapse::new(self.template.wrap);
        wfc.set_rules(rules);

        let wfc_size = if let Some(first_tile) = tiles.first() {
            let tile_size = first_tile.size();

            Size::new(
                self.template.size.width / tile_size.width,
                self.template.size.height / tile_size.height,
            )
        } else {
            Size::new(1, 1)
        };

        wfc.generate_map(&wfc_size, |_| {}, random_numbers);

        // render tiles into resulting land array
        let mut result = land2d::Land2D::new(&self.template.size, parameters.zero);
        let offset_y = result.height() - result.play_height() as usize;
        let offset_x = (result.width() - result.play_width() as usize) / 2;

        for row in 0..wfc_size.height as usize {
            for column in 0..wfc_size.width as usize {
                if let Some(Tile::Numbered(tile_index)) = wfc.grid().get(row, column) {
                    let tile = &tiles[*tile_index];

                    for tile_row in 0..tile.size().height as usize {
                        for tile_column in 0..tile.size().width as usize {
                            result.map(
                                (row * tile.size().height as usize + tile_row + offset_y) as i32,
                                (column * tile.size().width as usize + tile_column + offset_x)
                                    as i32,
                                |p| {
                                    *p =
                                        *tile.get(tile_row, tile_column).unwrap_or(&parameters.zero)
                                },
                            );
                        }
                    }
                } else {
                    // couldn't find a tile to place here, dump some debug info for tile set maker
                    let mut edges = ["-", "|", "-", "|"].map(|s| s.to_owned());

                    if row > 0 {
                        let tile = wfc.grid().get(row - 1, column);
                        edges[0] = if let Some(Tile::Numbered(tile_index)) = tile {
                            tiles[*tile_index].edge_set().bottom().name()
                        } else {
                            format!("{:?}", tile.unwrap())
                        }
                    }
                    if column < wfc_size.width as usize - 1 {
                        let tile = wfc.grid().get(row, column + 1);
                        edges[1] = if let Some(Tile::Numbered(tile_index)) = tile {
                            tiles[*tile_index].edge_set().left().name()
                        } else {
                            format!("{:?}", tile.unwrap())
                        }
                    }
                    if row < wfc_size.height as usize - 1 {
                        let tile = wfc.grid().get(row + 1, column);
                        edges[2] = if let Some(Tile::Numbered(tile_index)) = tile {
                            tiles[*tile_index].edge_set().top().name()
                        } else {
                            format!("{:?}", tile.unwrap())
                        }
                    }
                    if column > 0 {
                        let tile = wfc.grid().get(row, column - 1);
                        edges[3] = if let Some(Tile::Numbered(tile_index)) = tile {
                            tiles[*tile_index].edge_set().right().name()
                        } else {
                            format!("{:?}", tile.unwrap())
                        }
                    }
                    eprintln!(
                        "Couldn't find a tile to place here (row, column): ({}, {}), edges are: [{}]",
                        row, column, edges.join(", "),
                    );
                }
            }
        }

        result
    }
}

impl From<&EdgeDescription> for Edge<String> {
    fn from(val: &EdgeDescription) -> Self {
        let edge = Edge::new(
            val.name.clone(),
            val.symmetrical.unwrap_or_default(),
            val.hard_match.unwrap_or_default(),
        );

        if val.reversed.unwrap_or_default() {
            edge.reversed()
        } else {
            edge
        }
    }
}

impl<T: AsRef<str>> From<T> for EdgeDescription {
    fn from(val: T) -> Self {
        use std::cmp::Ordering;

        let mut chars = val.as_ref().chars();
        let hard_match = chars.next() == Some('!');

        let (name, reversed): (String, String) = if hard_match {
            (chars.clone().collect(), chars.rev().collect())
        } else {
            (
                val.as_ref().chars().collect(),
                val.as_ref().chars().rev().collect(),
            )
        };

        match name.cmp(&reversed) {
            Ordering::Less => EdgeDescription {
                name,
                symmetrical: Some(false),
                reversed: Some(false),
                hard_match: Some(hard_match),
            },
            Ordering::Equal => EdgeDescription {
                name: reversed,
                symmetrical: Some(true),
                reversed: Some(false),
                hard_match: Some(hard_match),
            },
            Ordering::Greater => EdgeDescription {
                name: reversed,
                symmetrical: Some(false),
                reversed: Some(true),
                hard_match: Some(hard_match),
            },
        }
    }
}
