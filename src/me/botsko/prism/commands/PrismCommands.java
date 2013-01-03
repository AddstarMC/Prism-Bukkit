package me.botsko.prism.commands;

import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.Executor;
import me.botsko.prism.commandlibs.SubHandler;

public class PrismCommands extends Executor {

	
	/**
	 * 
	 * @param prism
	 */
	public PrismCommands(Prism prism) {
		super(prism);
		setupCommands();
	}
	

	/**
	 * 
	 */
	private void setupCommands() {

		// @todo example
		addSub("subcmd", "example.use")
		.setMinArgs(1)
		.setUsage("<player>")
		.setDescription("desc")
		.setHandler(new SubHandler() {
			public void handle(CallInfo call) {

			}
		});
	}
}
