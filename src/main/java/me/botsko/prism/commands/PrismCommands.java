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
		super( prism, "subcommand", "prism" );
		setupCommands();
	}
	

	/**
	 * 
	 */
	private void setupCommands() {
		
		final Prism prism = (Prism) plugin;
		
		/**
		 * /prism about
		 */
		addSub("about", "prism.help")
		.setDescription("About Prism")
		.allowConsole()
		.setHandler(new AboutCommand(prism));
		

		/**
		 * /prism lookup 
		 */
		addSub("lookup", "prism.lookup")
		.setMinArgs(1)
		.setUsage("(params)")
		.setDescription("Search for actions.")
		.addAlias("l")
		.setHandler(new LookupCommand(prism));
		
		/**
		 * /prism near 
		 */
		addSub("near", "prism.lookup")
		.setUsage("(params)")
		.setDescription("Search for actions in a small radius around you.")
		.setHandler(new NearCommand(prism));
		
		/**
		 * /prism page [page] 
		 */
		addSub("page", "prism.lookup")
		.setMinArgs(1)
		.setUsage("[page number]")
		.setDescription("Displays [page] of the most recent search results.")
		.addAlias("pg")
		.setHandler(new PageCommand(prism));
		
		/**
		 * /prism i
		 */
		addSub("inspect", "prism.lookup")
		.setDescription("Toggles the block inspection tool to your hand.")
		.addAlias("i")
		.setHandler(new InspectCommand(prism));
		
		/**
		 * /prism wand
		 */
		addSub("wand", "prism.rollback")
		.addAlias("w")
		.setDescription("Toggles the wand.")
		.setHandler(new WandCommand(prism));
		
		/**
		 * /prism teleport [id] 
		 */
		addSub("tp", "prism.tp")
		.setMinArgs(1)
		.setUsage("[record id]")
		.setDescription("Teleport to record [id].")
		.setHandler(new TeleportCommand(prism));
		
		/**
		 * /prism ext
		 */
		addSub("ex", "prism.extinguish")
		.setUsage("(radius)")
		.setDescription("Puts out any fire within (radius)")
		.setHandler(new ExtinguishCommand(prism));
		
		/**
		 * /prism drain
		 */
		addSub("drain", "prism.drain")
		.setUsage("(radius)")
		.setDescription("Drains lava and water within (radius)")
		.setHandler(new DrainCommand(prism));
		
		/**
		 * /prism preview (params)
		 */
		addSub("preview", "prism.preview")
		.setMinArgs(1)
		.setUsage("(params)")
		.setDescription("Preview a rollback for (params).")
		.setHandler(new PreviewCommand(prism));
		
		/**
		 * /prism report
		 */
		addSub("report", "prism.report")
		.addAlias("rp")
		.setDescription("Reports")
		.allowConsole()
		.setHandler(new ReportCommand(prism));
		
		/**
		 * /prism rollback (params)
		 */
		addSub("rollback", "prism.rollback")
		.setMinArgs(1)
		.setUsage("(params)")
		.setDescription("Rollback actions for (params).")
		.setHandler(new RollbackCommand(prism));
		
		/**
		 * /prism restore (params)
		 */
		addSub("restore", "prism.restore")
		.setMinArgs(1)
		.setUsage("(params)")
		.setDescription("Re-applies actions for (params).")
		.setHandler(new RestoreCommand(prism));
		
		/**
		 * /prism delete (time)
		 */
		addSub("delete", "prism.delete")
		.allowConsole()
		.setDescription("Deletes all records from the database before (timeframe)")
		.setHandler(new DeleteCommand(prism));
		
		/**
		 * /prism undo
		 */
		addSub("undo", "prism.rollback")
		.setDescription("Revert a previous process.")
		.setHandler(new UndoCommand(prism));
		
		/**
		 * /prism ?
		 */
		addSub("?", "prism.help")
		.addAlias("help")
		.allowConsole()
		.setDescription("This. Helpception!")
		.setHandler(new HelpCommand(prism));
		
		/**
		 * /prism params
		 */
		addSub("params", "prism.help")
		.allowConsole()
		.setDescription("Parameter help.")
		.setHandler(new ParamsCommand(prism));
		
		/**
		 * /prism reload
		 */
		addSub("reload", "prism.reload")
		.allowConsole()
		.setDescription("Reloads the configuration files.")
		.setHandler(new SubHandler() {
            public void handle(CallInfo call) {
            	prism.reloadConfig();
            	prism.config = prism.getConfig();
				call.getSender().sendMessage( prism.messenger.playerMsg("Configuration reloaded successfully.") );
            }
		});
	}
}