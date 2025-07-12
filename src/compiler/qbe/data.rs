use std::fmt;

use crate::ensure_ascii;

use super::{data_item::DataItem, linkage::Linkage, r#type::Type};

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct Data {
    pub linkage: Linkage,
    pub name: String,
    pub align: Option<u64>,
    pub items: Vec<(Type, DataItem)>,
}

impl Data {
    pub const fn new(
        linkage: Linkage,
        name: String,
        align: Option<u64>,
        items: Vec<(Type, DataItem)>,
    ) -> Self {
        Self {
            linkage,
            name,
            align,
            items,
        }
    }
}

impl fmt::Display for Data {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(
            formatter,
            "{}data ${} = ",
            self.linkage,
            ensure_ascii!(self.name.clone(), wrap)
        )?;

        if let Some(align) = self.align {
            write!(formatter, "align {align} ")?;
        }
        write!(
            formatter,
            "{{ {} }}",
            self.items
                .iter()
                .map(|(ty, item)| format!("{ty} {item}"))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}
