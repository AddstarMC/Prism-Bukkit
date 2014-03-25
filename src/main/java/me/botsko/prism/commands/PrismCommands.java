package me.botsko.prism.commands;

import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.Executor;
import me.botsko.prism.commandlibs.SubHandler;

import java.util.List;

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
        addSub( new String[] { "about", "default" }, "prism.help" ).allowConsole()
                .setHandler( new AboutCommand( prism ) );

        /**
         * /prism lookup
         */
        addSub( new String[] { "lookup", "l" }, "prism.lookup" ).allowConsole().setMinArgs( 1 )
                .setHandler( new LookupCommand( prism ) );

        /**
         * /prism near
         */
        addSub( "near", "prism.lookup" ).setHandler( new NearCommand( prism ) );

        /**
         * /prism page [page]
         */
        addSub( new String[] { "page", "pg" }, new String[] { "prism.lookup.paginate", "prism.lookup" } )
                .allowConsole().setMinArgs( 1 ).setHandler( new PageCommand( prism ) );

        /**
         * /prism wand
         */
        addSub(
                new String[] { "wand", "w", "i", "inspect" },
                new String[] { "prism.rollback", "prism.restore", "prism.lookup", "prism.wand.inspect",
                        "prism.wand.profile", "prism.wand.rollback", "prism.wand.restore" } ).setHandler(
                new WandCommand( prism ) );

        /**
         * /prism setmy
         */
        addSub( new String[] { "setmy" }, new String[] { "prism.setmy.wand" } ).setHandler( new SetmyCommand( prism ) );

        /**
         * /prism resetmy
         */
        addSub( new String[] { "resetmy" }, new String[] { "prism.setmy.wand" } ).setHandler(
                new ResetmyCommand( prism ) );

        /**
         * /prism teleport [id]
         */
        addSub( "tp", "prism.tp" ).setMinArgs( 1 ).setHandler( new TeleportCommand( prism ) );

        /**
         * /prism ext
         */
        addSub( "ex", "prism.extinguish" ).setHandler( new ExtinguishCommand( prism ) );

        /**
         * /prism drain
         */
        addSub( "drain", "prism.drain" ).setHandler( new DrainCommand( prism ) );

        /**
         * /prism preview (params)
         */
        addSub( new String[] { "preview", "pv" }, "prism.preview" ).setMinArgs( 1 ).setHandler(
                new PreviewCommand( prism ) );

        /**
         * /prism report
         */
        addSub( new String[] { "report", "rp" }, "prism.report" ).allowConsole()
                .setHandler( new ReportCommand( prism ) );

        /**
         * /prism rollback (params)
         */
        addSub( new String[] { "rollback", "rb" }, "prism.rollback" ).allowConsole().setMinArgs( 1 )
                .setHandler( new RollbackCommand( prism ) );

        /**
         * /prism restore (params)
         */
        addSub( new String[] { "restore", "rs" }, "prism.restore" ).allowConsole().setMinArgs( 1 )
                .setHandler( new RestoreCommand( prism ) );

        /**
         * /prism delete (time)
         */
        addSub( new String[] { "delete", "purge" }, "prism.delete" ).allowConsole().setHandler(
                new DeleteCommand( prism ) );

        /**
         * /prism recorder
         */
        addSub( "recorder", "prism.recorder" ).allowConsole().setHandler( new RecorderCommand( prism ) );

        /**
         * /prism undo
         */
        addSub( "undo", "prism.rollback" ).setHandler( new UndoCommand( prism ) );

        /**
         * /prism view
         */
        addSub( new String[] { "view", "v" }, "prism.view" ).setMinArgs( 1 ).setHandler( new ViewCommand( prism ) );

        /**
         * /prism ?
         */
        addSub( new String[] { "help", "?" }, "prism.help" ).allowConsole().setHandler( new HelpCommand() );

        /**
         * /prism params
         */
        addSub( "params", "prism.help" ).allowConsole().setHandler( new ParamsCommand() );

        /**
         * /prism actions
         */
        addSub( "actions", "prism.help" ).allowConsole().setHandler( new ActionsCommand() );

        /**
         * /prism flags
         */
        addSub( "flags", "prism.help" ).allowConsole().setHandler( new FlagsCommand() );

        /**
         * /prism reload
         */
        addSub( "reload", "prism.reload" ).allowConsole().setHandler( new SubHandler() {
            @Override
            public void handle(CallInfo call) {
                prism.reloadConfig();
                call.getSender()
                        .sendMessage( Prism.messenger.playerHeaderMsg( "Configuration reloaded successfully." ) );
            }

            @Override
            public List<String> handleComplete(CallInfo call) {
                return null;
            }
        } );
    }

}
