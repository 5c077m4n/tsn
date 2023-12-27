mod libs;

use anyhow::Result;
use libs::repl::start;

fn main() -> Result<()> {
	start()
}
