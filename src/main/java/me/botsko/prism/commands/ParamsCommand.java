package me.botsko.prism.commands;

import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;

import java.util.List;

public class ParamsCommand implements SubHandler {

    /**
     * Handle the command
     */
    @Override
    public void handle(CallInfo call) {
        help( call.getSender() );
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return null;
    }

    /**
     * Display param help
     * 
     * @param sender
     */
    private void help(CommandSender sender) {

        sender.sendMessage( Prism.messenger.playerHeaderMsg( ChatColor.GOLD + "--- Parameters Help ---" ) );

        sender.sendMessage( Prism.messenger.playerMsg( ChatColor.LIGHT_PURPLE + "r:[radius]" + ChatColor.WHITE
                + " i.e. 20, or 100. Defaults to default-radius defined in config." ) );
        sender.sendMessage( Prism.messenger.playerMsg( ChatColor.LIGHT_PURPLE + "r:global" + ChatColor.WHITE
                + " Force a worldwide search, for lookups only (unless configured for rollbacks)." ) );
        sender.sendMessage( Prism.messenger.playerMsg( ChatColor.LIGHT_PURPLE + "r:<player|x,y,z>:[radius]"
                + ChatColor.WHITE
                + " Base the radius around another place, like r:viveleroi:20 or r:20,35,10:5 (x,y,z)" ) );
        sender.sendMessage( Prism.messenger.playerMsg( ChatColor.LIGHT_PURPLE + "r:we" + ChatColor.WHITE
                + " Use a WorldEdit selection." ) );

        sender.sendMessage( Prism.messenger.playerMsg( ChatColor.GRAY + "---" ) );

        sender.sendMessage( Prism.messenger.playerMsg( ChatColor.LIGHT_PURPLE + "a:[action]" + ChatColor.WHITE
                + " Like 'block-break' (See below for full list). No default." ) );
        sender.sendMessage( Prism.messenger.playerMsg( ChatColor.LIGHT_PURPLE + "b:[block]" + ChatColor.WHITE
                + " Like 'grass' or '2' or '2:0'. No default." ) );
        sender.sendMessage( Prism.messenger.playerMsg( ChatColor.LIGHT_PURPLE + "before:[time]" + ChatColor.WHITE
                + " Events prior to x long ago." ) );
        sender.sendMessage( Prism.messenger.playerMsg( ChatColor.LIGHT_PURPLE + "e:[entity]" + ChatColor.WHITE
                + " Like 'pig'. No default." ) );
        sender.sendMessage( Prism.messenger.playerMsg( ChatColor.LIGHT_PURPLE + "id:[#]" + ChatColor.WHITE
                + " Record id. Useful for single item rollbacks/restores without a wand." ) );
        sender.sendMessage( Prism.messenger.playerMsg( ChatColor.LIGHT_PURPLE + "k:[text]" + ChatColor.WHITE
                + " Keyword search. Mainly for command/chat logging." ) );
        sender.sendMessage( Prism.messenger.playerMsg( ChatColor.LIGHT_PURPLE + "p:[player]" + ChatColor.WHITE
                + " Like 'viveleroi'. No default." ) );
        sender.sendMessage( Prism.messenger.playerMsg( ChatColor.LIGHT_PURPLE + "since:[time]" + ChatColor.WHITE
                + " Events since to x long ago (same as t:)." ) );
        sender.sendMessage( Prism.messenger
                .playerMsg( ChatColor.LIGHT_PURPLE
                        + "t:[time]"
                        + ChatColor.WHITE
                        + " Events since x long ago. Like 1s(seconds), 20m(minutes), 1h(hour), 7d(days), 2w(weeks). Default based on config." ) );
        sender.sendMessage( Prism.messenger.playerMsg( ChatColor.LIGHT_PURPLE + "w:[world]" + ChatColor.WHITE
                + " Defaults to your current world." ) );
        sender.sendMessage( Prism.messenger
                .playerMsg( "Prefix action, player, entity names with ! to exclude. Like p:!viveleroi" ) );
        sender.sendMessage( Prism.messenger.playerMsg( "Prefix player names with ~ for partial match. Like p:~vive" ) );

        sender.sendMessage( Prism.messenger.playerMsg( ChatColor.GRAY + "Use " + ChatColor.WHITE + "/pr actions"
                + ChatColor.GRAY + " to view list of actions." ) );

    }
}