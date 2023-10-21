#[repr(u8)]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum SpecId {
    FRONTIER = 0,         // Frontier               0
    FRONTIER_THAWING = 1, // Frontier Thawing       200000
    HOMESTEAD = 2,        // Homestead              1150000
    DAO_FORK = 3,         // DAO Fork               1920000
    TANGERINE = 4,        // Tangerine Whistle      2463000
    SPURIOUS_DRAGON = 5,  // Spurious Dragon        2675000
    BYZANTIUM = 6,        // Byzantium              4370000
    CONSTANTINOPLE = 7,   // Constantinople         7280000 is overwritten with PETERSBURG
    PETERSBURG = 8,       // Petersburg             7280000
    ISTANBUL = 9,         // Istanbul               9069000
    MUIR_GLACIER = 10,    // Muir Glacier           9200000
    BERLIN = 11,          // Berlin                 12244000
    LONDON = 12,          // London                 12965000
    ARROW_GLACIER = 13,   // Arrow Glacier          13773000
    GRAY_GLACIER = 14,    // Gray Glacier           15050000
    MERGE = 15,           // Paris/Merge            15537394 (TTD: 58750000000000000000000)
    SHANGHAI = 16,        // Shanghai               17034870 (TS: 1681338455)
    CANCUN = 17,          // Cancun                 TBD
    #[cfg(feature = "optimism")]
    BEDROCK = 128,
    #[cfg(feature = "optimism")]
    REGOLITH = 129,
    LATEST = u8::MAX,
}
