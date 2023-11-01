use alloy_primitives::{address, Address, B160, B256, U256};
use revm_primitives::{
    db::Database, hash_map::Entry, Account, AccountInfo, Bytecode, State, StorageSlot,
};

pub type DBBox<'a, E> = Box<dyn Database<Error = E> + Send + 'a>;

#[derive(Debug)]
pub struct EVMState {
    pub state: State,
    db: *const (),
    basic: unsafe fn(*const (), Address) -> Option<AccountInfo>,
    code_by_hash: unsafe fn(*const (), B256) -> Bytecode,
    storage: unsafe fn(*const (), Address, U256) -> U256,
    block_hash: unsafe fn(*const (), U256) -> B256,
}

impl EVMState {
    pub fn with_db<DB: Database>(db: &DB) -> EVMState {
        EVMState {
            state: Default::default(),
            db: db as *const _ as *const (),
            basic: basic::<DB>,
            code_by_hash: code_by_hash::<DB>,
            storage: storage::<DB>,
            block_hash: block_hash::<DB>,
        }
    }

    // TODO: gas accounting
    pub fn load_account(&mut self, address: Address) -> (&mut Account, bool) {
        match self.state.entry(address) {
            Entry::Occupied(entry) => (entry.into_mut(), true),
            Entry::Vacant(entry) => {
                let account = unsafe { (self.basic)(self.db, address) };
                let account = match account {
                    Some(account) => account.into(),
                    None => Account::new_not_existing(),
                };

                // TODO: check if it's precompile for is_warm flag
                //       journaling for reverts.
                (entry.insert(account), false)
            }
        }
    }

    // TODO: gas accounting
    pub fn sload(&mut self, address: Address, index: U256) -> (U256, bool) {
        let account = self.state.get_mut(&address).unwrap();

        match account.storage.entry(index) {
            Entry::Occupied(entry) => (entry.get().present_value, true),
            Entry::Vacant(entry) => {
                let item = unsafe { (self.storage)(self.db, address, index) };
                entry.insert(StorageSlot::new(item));
                (item, false)
            }
        }
    }

    pub fn sstore(
        &mut self,
        address: Address,
        index: U256,
        value: U256,
    ) -> (U256, U256, U256, bool) {
        let (current, warm) = self.sload(address, index);

        // Expect these to be warm by now.
        let account = self.state.get_mut(&address).unwrap();
        let slot = account.storage.get_mut(&index).unwrap();

        if current == value {
            return (slot.previous_or_original_value, current, value, warm);
        }

        // TODO: journaling a storage change
        slot.present_value = value;

        (slot.previous_or_original_value, current, value, warm)
    }
}

unsafe fn basic<DB: Database>(db: *const (), address: Address) -> Option<AccountInfo> {
    let db = &mut *(db as *mut DB);

    match db.basic(address) {
        Ok(r) => r,
        Err(e) => {
            panic!("Could not fetch basic account info from DB!")
        }
    }
}

unsafe fn code_by_hash<DB: Database>(db: *const (), hash: B256) -> Bytecode {
    let db = &mut *(db as *mut DB);

    match db.code_by_hash(hash) {
        Ok(r) => r,
        Err(e) => {
            panic!("Could not fetch code from DB!")
        }
    }
}

unsafe fn storage<DB: Database>(db: *const (), address: Address, index: U256) -> U256 {
    let db = &mut *(db as *mut DB);

    match db.storage(address, index) {
        Ok(r) => r,
        Err(e) => {
            panic!("Could not fetch storage from DB!")
        }
    }
}

unsafe fn block_hash<DB: Database>(db: *const (), block: U256) -> B256 {
    let db = &mut *(db as *mut DB);

    match db.block_hash(block) {
        Ok(r) => r,
        Err(e) => {
            panic!("Could not fetch block hash from DB!")
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use alloy_primitives::{uint, Uint};
    use revm::db::InMemoryDB;

    const ADDRESS: Address = address!("3fC91A3afd70395Cd496C647d5a6CC9D4B2b7FAD");
    const ADDRESS_EMPTY: Address = address!("d4E96eF8eee8678dBFf4d535E033Ed1a4F7605b7");
    const CONTRACT: &str =
        "6175305b8015610026578060200a60300a60400a60500a60600a60700a5060019003610003565b5000";
    const ITEM0: U256 = uint!(4000_U256);
    const ITEM5: U256 = uint!(700_U256);
    const ITEM9: U256 = uint!(4242_U256);

    fn init_db() -> InMemoryDB {
        let mut db = InMemoryDB::default();
        let slice = hex::decode(CONTRACT).expect("Could not parse contract hex");
        let bytecode = Bytecode::new_raw(slice.into()).to_checked();

        let info = AccountInfo::new(U256::from(42069), 2000, bytecode.hash_slow(), bytecode);

        db.insert_account_info(ADDRESS, info);
        db.insert_account_storage(ADDRESS, U256::ZERO, ITEM0)
            .expect("Failed to insert storage");
        db.insert_account_storage(ADDRESS, U256::from(5), ITEM5)
            .expect("Failed to insert storage");
        db.insert_account_storage(ADDRESS, U256::from(9), ITEM9)
            .expect("Failed to insert storage");

        db
    }

    #[test]
    fn test_db_sload_sstore() {
        let db = init_db();

        let mut evm_state = EVMState::with_db(&db);
        let _ = evm_state.load_account(ADDRESS);
        let _ = evm_state.load_account(ADDRESS_EMPTY);

        let (item0, warm) = evm_state.sload(ADDRESS, U256::ZERO);
        assert_eq!(item0, ITEM0);
        assert_eq!(warm, false);

        let (item5, warm) = evm_state.sload(ADDRESS, U256::from(5));
        assert_eq!(item5, ITEM5);
        assert_eq!(warm, false);

        let (item_empty, warm) = evm_state.sload(ADDRESS, U256::from(99));
        assert_eq!(item_empty, U256::ZERO);
        assert_eq!(warm, false);

        let (item_empty, warm) = evm_state.sload(ADDRESS_EMPTY, U256::ZERO);
        assert_eq!(item_empty, U256::ZERO);
        assert_eq!(warm, false);

        let (item0, warm) = evm_state.sload(ADDRESS, U256::ZERO);
        assert_eq!(item0, ITEM0);
        assert_eq!(warm, true);

        let (item5, warm) = evm_state.sload(ADDRESS, U256::from(5));
        assert_eq!(item5, ITEM5);
        assert_eq!(warm, true);

        let (item9orig, item9current, item9new, warm) =
            evm_state.sstore(ADDRESS, U256::from(9), U256::from(2424));
        assert_eq!(item9orig, ITEM9);
        assert_eq!(item9current, ITEM9);
        assert_eq!(item9new, U256::from(2424));
        assert_eq!(warm, false);

        let (item9orig, item9current, item9new, warm) =
            evm_state.sstore(ADDRESS, U256::from(9), U256::from(99));
        assert_eq!(item9orig, ITEM9);
        assert_eq!(item9current, U256::from(2424));
        assert_eq!(item9new, U256::from(99));
        assert_eq!(warm, true);

        let (item9orig, item9current, item9new, warm) =
            evm_state.sstore(ADDRESS, U256::from(9), U256::from(99));
        assert_eq!(item9orig, ITEM9);
        assert_eq!(item9current, U256::from(99));
        assert_eq!(item9new, U256::from(99));
        assert_eq!(warm, true);
    }
}
