use std::fmt;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Default)]
pub struct Linkage {
    pub exported: bool,
    pub section: Option<String>,
    pub secflags: Option<String>,
}

impl Linkage {
    pub fn private() -> Linkage {
        Linkage {
            exported: false,
            section: None,
            secflags: None,
        }
    }

    pub fn public() -> Linkage {
        Linkage {
            exported: true,
            section: None,
            secflags: None,
        }
    }
}

impl fmt::Display for Linkage {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        if self.exported {
            write!(formatter, "export ")?;
        }
        if let Some(section) = &self.section {
            write!(formatter, "section \"{}\"", section)?;

            if let Some(secflags) = &self.secflags {
                write!(formatter, " \"{}\"", secflags)?;
            }

            write!(formatter, " ")?;
        }

        Ok(())
    }
}
