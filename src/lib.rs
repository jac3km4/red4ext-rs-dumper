use std::fs::File;
use std::io::BufWriter;

use dumper::Dumper;
use red4ext_rs::{
    export_plugin, log, wcstr, GameApp, Plugin, RttiSystem, SdkEnv, SemVer, StateListener,
    StateType, U16CStr,
};

mod dumper;

pub struct DumperPlugin;

impl Plugin for DumperPlugin {
    const AUTHOR: &'static U16CStr = wcstr!("jekky");
    const NAME: &'static U16CStr = wcstr!("red4ext-rs-dumper");
    const VERSION: SemVer = SemVer::new(0, 1, 0);

    fn on_init(env: &SdkEnv) {
        env.add_listener(
            StateType::Running,
            StateListener::default().with_on_enter(on_game_running),
        );
    }
}

export_plugin!(DumperPlugin);

unsafe extern "C" fn on_game_running(_app: &GameApp) {
    if let Err(err) = dump() {
        log::info!("Error during dump: {:?}", err);
    }
}

fn dump() -> anyhow::Result<()> {
    let mut output = BufWriter::new(File::create("generated.rs")?);
    let rtti = RttiSystem::get();
    let dumper = Dumper::new(&rtti)?;
    log::info!("{}", dumper.report());
    dumper.write(&mut output)
}
