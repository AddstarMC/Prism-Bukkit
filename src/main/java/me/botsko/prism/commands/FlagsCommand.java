package me.botsko.prism.commands;

import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.Flag;
import me.botsko.prism.commandlibs.SubHandler;
import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;

import java.util.List;

public class FlagsCommand implements SubHandler {

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

        sender.sendMessage( Prism.messenger.playerHeaderMsg( ChatColor.GOLD + "--- Flags Help ---" ) );

        sender.sendMessage( Prism.messenger.playerMsg( ChatColor.GRAY
                + "Flags control how Prism applies a rollback/restore, or formats lookup results." ) );
        sender.sendMessage( Prism.messenger.playerMsg( ChatColor.GRAY
                + "Use them after parameters, like /pr l p:viveleroi -extended" ) );
        for ( final Flag flag : Flag.values() ) {
            sender.sendMessage( Prism.messenger.playerMsg( ChatColor.LIGHT_PURPLE + flag.getUsage().replace( "_", "-" )
                    + ChatColor.WHITE + " " + flag.getDescription() ) );
        }
    }
}