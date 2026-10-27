use std::fs::File;
use std::io::BufWriter;

use dumper::Dumper;
use red4ext_rs::{
    export_plugin_symbols, log, wcstr, GameApp, Plugin, RttiSystem, SdkEnv, SemVer,
    StateHandlerResult, StateListener, StateType, U16CStr,
};

mod dumper;
mod layout;

#[derive(Debug)]
pub struct DumperPlugin;

impl Plugin for DumperPlugin {
    const AUTHOR: &'static U16CStr = wcstr!("jekky");
    const NAME: &'static U16CStr = wcstr!("red4ext-rs-dumper");
    const VERSION: SemVer = SemVer::new(0, 1, 0);

    fn on_load(env: &SdkEnv) {
        env.add_listener(
            StateType::Running,
            StateListener::default().with_on_enter(on_game_running),
        );
    }
}

export_plugin_symbols!(DumperPlugin);

unsafe extern "C" fn on_game_running(_app: &GameApp) -> StateHandlerResult {
    if let Err(err) = dump() {
        log::error!("Error during dump: {:?}", err);
    }
    StateHandlerResult::Finished
}

fn dump() -> anyhow::Result<()> {
    let rtti = RttiSystem::get();
    let dumper = Dumper::new(&rtti);
    log::info!("{dumper}");

    dumper.write(&mut BufWriter::new(File::create("generated.rs")?))
}
