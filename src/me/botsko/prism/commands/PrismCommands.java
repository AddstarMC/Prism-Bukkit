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
		
		
		/**
		 * /prism about
		 */
		addSub("about", "prism.help")
		.setDescription("About Prism")
		.allowConsole()
		.setHandler(new AboutCommand(plugin));
		

		/**
		 * /prism lookup 
		 */
		addSub("lookup", "prism.lookup")
		.setMinArgs(1)
		.setUsage("(params)")
		.setDescription("Search for actions.")
		.addAlias("l")
		.setHandler(new LookupCommand(plugin));
		
		/**
		 * /prism near 
		 */
		addSub("near", "prism.lookup")
		.setUsage("(params)")
		.setDescription("Search for actions in a small radius around you.")
		.setHandler(new NearCommand(plugin));
		
		/**
		 * /prism page [page] 
		 */
		addSub("page", "prism.lookup")
		.setMinArgs(1)
		.setUsage("[page number]")
		.setDescription("Displays [page] of the most recent search results.")
		.addAlias("pg")
		.setHandler(new PageCommand(plugin));
		
		/**
		 * /prism i
		 */
		addSub("inspect", "prism.lookup")
		.setDescription("Toggles the block inspection tool to your hand.")
		.addAlias("i")
		.setHandler(new InspectCommand(plugin));
		
		/**
		 * /prism wand
		 */
		addSub("wand", "prism.rollback")
		.addAlias("w")
		.setDescription("Toggles the wand.")
		.setHandler(new WandCommand(plugin));
		
		/**
		 * /prism teleport [id] 
		 */
		addSub("tp", "prism.tp")
		.setMinArgs(1)
		.setUsage("[record id]")
		.setDescription("Teleport to record [id].")
		.setHandler(new TeleportCommand(plugin));
		
		/**
		 * /prism ext
		 */
		addSub("ex", "prism.extinguish")
		.setUsage("(radius)")
		.setDescription("Puts out any fire within (radius)")
		.setHandler(new ExtinguishCommand(plugin));
		
		/**
		 * /prism drain
		 */
		addSub("drain", "prism.drain")
		.setUsage("(radius)")
		.setDescription("Drains lava and water within (radius)")
		.setHandler(new DrainCommand(plugin));
		
		/**
		 * /prism preview (params)
		 */
		addSub("preview", "prism.preview")
		.setMinArgs(1)
		.setUsage("(params)")
		.setDescription("Preview a rollback for (params).")
		.setHandler(new PreviewCommand(plugin));
		
		/**
		 * /prism rollback (params)
		 */
		addSub("rollback", "prism.rollback")
		.setMinArgs(1)
		.setUsage("(params)")
		.setDescription("Rollback actions for (params).")
		.setHandler(new RollbackCommand(plugin));
		
		/**
		 * /prism restore (params)
		 */
		addSub("restore", "prism.restore")
		.setMinArgs(1)
		.setUsage("(params)")
		.setDescription("Re-applies actions for (params).")
		.setHandler(new RestoreCommand(plugin));
		
		/**
		 * /prism delete (time)
		 */
		addSub("delete", "prism.delete")
		.setUsage("(timeframe)")
		.setDescription("Deletes all records from the database before (timeframe)")
		.setHandler(new DeleteCommand(plugin));
		
		/**
		 * /prism undo
		 */
		addSub("undo", "prism.rollback")
		.setDescription("Revert a previous process.")
		.setHandler(new UndoCommand(plugin));
		
		/**
		 * /prism ?
		 */
		addSub("?", "prism.help")
		.addAlias("help")
		.allowConsole()
		.setDescription("This. Helpception!")
		.setHandler(new HelpCommand(plugin));
		
		/**
		 * /prism params
		 */
		addSub("params", "prism.help")
		.allowConsole()
		.setDescription("Parameter help.")
		.setHandler(new ParamsCommand(plugin));
		
		/**
		 * /prism reload
		 */
		addSub("reload", "prism.reload")
		.allowConsole()
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