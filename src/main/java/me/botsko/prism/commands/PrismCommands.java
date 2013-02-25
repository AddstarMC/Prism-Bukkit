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
		.allowConsole()
		.setHandler(new AboutCommand(prism));
		

		/**
		 * /prism lookup 
		 */
		addSub(new String[]{"lookup","l"}, "prism.lookup")
		.allowConsole()
		.setMinArgs(1)
		.setHandler(new LookupCommand(prism));
		
		/**
		 * /prism near 
		 */
		addSub("near", "prism.lookup")
		.setHandler(new NearCommand(prism));
		
		/**
		 * /prism page [page] 
		 */
		addSub( new String[]{"page","pg"}, "prism.lookup")
		.allowConsole()
		.setMinArgs(1)
		.setHandler(new PageCommand(prism));
		
		/**
		 * /prism i
		 */
		addSub( new String[]{"inspect","i"}, "prism.lookup")
		.setHandler(new InspectCommand(prism));
		
		/**
		 * /prism wand
		 */
		addSub( new String[]{"wand","w"}, new String[]{"prism.rollback","prism.restore","prism.lookup"} )
		.setHandler(new WandCommand(prism));
		
		/**
		 * /prism teleport [id] 
		 */
		addSub("tp", "prism.tp")
		.setMinArgs(1)
		.setHandler(new TeleportCommand(prism));
		
		/**
		 * /prism ext
		 */
		addSub("ex", "prism.extinguish")
		.setHandler(new ExtinguishCommand(prism));
		
		/**
		 * /prism drain
		 */
		addSub("drain", "prism.drain")
		.setHandler(new DrainCommand(prism));
		
		/**
		 * /prism preview (params)
		 */
		addSub(new String[]{"preview","pv"}, "prism.preview")
		.setMinArgs(1)
		.setHandler(new PreviewCommand(prism));
		
		/**
		 * /prism report
		 */
		addSub( new String[]{"report","rp"}, "prism.report")
		.allowConsole()
		.setHandler(new ReportCommand(prism));
		
		/**
		 * /prism rollback (params)
		 */
		addSub( new String[]{"rollback","rb"}, "prism.rollback")
		.setMinArgs(1)
		.setHandler(new RollbackCommand(prism));
		
		/**
		 * /prism restore (params)
		 */
		addSub( new String[]{"restore","rs"}, "prism.restore")
		.setMinArgs(1)
		.setHandler(new RestoreCommand(prism));
		
		/**
		 * /prism delete (time)
		 */
		addSub("delete", "prism.delete")
		.allowConsole()
		.setHandler(new DeleteCommand(prism));
		
		/**
		 * /prism undo
		 */
		addSub("undo", "prism.rollback")
		.setHandler(new UndoCommand(prism));
		
		/**
		 * /prism ?
		 */
		addSub( new String[]{"help","?"}, "prism.help")
		.allowConsole()
		.setHandler(new HelpCommand(prism));
		
		/**
		 * /prism params
		 */
		addSub("params", "prism.help")
		.allowConsole()
		.setHandler(new ParamsCommand(prism));
		
		/**
		 * /prism reload
		 */
		addSub("reload", "prism.reload")
		.allowConsole()
		.setHandler(new SubHandler() {
            public void handle(CallInfo call) {
            	prism.reloadConfig();
            	prism.config = plugin.getConfig();
				call.getSender().sendMessage( prism.messenger.playerMsg("Configuration reloaded successfully.") );
            }
		});
	}
}