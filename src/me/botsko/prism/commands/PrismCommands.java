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
		
		// @todo add subcommand aliases

		/**
		 * /prism lookup 
		 */
		addSub("l", "prism.lookup")
		.setMinArgs(1)
		.setUsage("(params)")
		.setDescription("Search for actions.")
		.setHandler(new LookupCommand(plugin));
		
		/**
		 * /prism near 
		 */
		addSub("near", "prism.lookup")
		.setMinArgs(1)
		.setUsage("(params)")
		.setDescription("Search for actions in a small radius around you.")
		.setHandler(new NearCommand(plugin));
		
		/**
		 * /prism page [page] 
		 */
		addSub("page", "prism.lookup")
		.setMinArgs(2)
		.setUsage("[page number]")
		.setDescription("Displays [page] of the most recent search results.")
		.setHandler(new PageCommand(plugin));
		
		/**
		 * /prism i
		 */
		addSub("i", "prism.lookup")
		.setMinArgs(1)
		.setDescription("Toggles the block inspection tool to your hand.")
		.setHandler(new InspectCommand(plugin));
		
		/**
		 * /prism teleport [id] 
		 */
		addSub("tp", "prism.tp")
		.setMinArgs(2)
		.setUsage("[record id]")
		.setDescription("Teleport to recordd [id].")
		.setHandler(new TeleportCommand(plugin));
		
		/**
		 * /prism ext
		 */
		addSub("ex", "prism.extinguish")
		.setMinArgs(1)
		.setUsage("(radius)")
		.setDescription("Puts out any fire within (radius)")
		.setHandler(new ExtinguishCommand(plugin));
		
		/**
		 * /prism drain
		 */
		addSub("drain", "prism.drain")
		.setMinArgs(1)
		.setUsage("(radius)")
		.setDescription("Drains lava and water within (radius)")
		.setHandler(new ExtinguishCommand(plugin));
		
		/**
		 * /prism preview (params)
		 */
		addSub("preview", "prism.preview")
		.setMinArgs(2)
		.setUsage("(params)")
		.setDescription("Preview a rollback for (params).")
		.setHandler(new PreviewCommand(plugin));
		
		/**
		 * /prism rollback (params)
		 */
		addSub("rollback", "prism.rollback")
		.setMinArgs(2)
		.setUsage("(params)")
		.setDescription("Rollback actions for (params).")
		.setHandler(new RollbackCommand(plugin));
		
		/**
		 * /prism restore (params)
		 */
		addSub("restore", "prism.restore")
		.setMinArgs(2)
		.setUsage("(params)")
		.setDescription("Re-applies actions for (params).")
		.setHandler(new RestoreCommand(plugin));
		
		/**
		 * /prism delete (time)
		 */
		addSub("restore", "prism.delete")
		.setMinArgs(2)
		.setUsage("(timeframe)")
		.setDescription("Deletes all records from the database before (timeframe)")
		.setHandler(new RestoreCommand(plugin));
		
		/**
		 * /prism ?
		 */
		addSub("?", "prism.help")
		.setDescription("This. Helpception!")
		.setHandler(new HelpCommand(plugin));
		
		/**
		 * /prism reload
		 */
		addSub("reload", "prism.reload")
		.setDescription("Reloads the configuration files.")
		.setHandler(new SubHandler() {
            public void handle(CallInfo call) {
            	plugin.reloadConfig();
				plugin.config = plugin.getConfig();
				call.getSender().sendMessage( plugin.playerMsg("Configuration reloaded successfully.") );
            }
		});
	}
}