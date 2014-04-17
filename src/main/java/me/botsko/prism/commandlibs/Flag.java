package me.botsko.prism.commandlibs;

import org.bukkit.permissions.Permissible;

public enum Flag {
    DRAIN("Drain liquid along with a rollback."), DRAIN_LAVA("Drain only lava along with a rollback."), DRAIN_WATER(
            "Drain only water along with a rollback."), EXTENDED(
            "Shows the extended lookup results (timestamp, coords, id, etc)."), NO_EXT(
            "Do not extinguish fires on burn rollbacks."), NO_ITEMCLEAR("Do not clear drops on explosion rollback."), PER_PAGE(
            "-per-page=#", "Set results per-page for current lookup."), NO_GROUP(
            "Disables grouping of related actions."), OVERWRITE(
            "Forces rb/rs to not skip blocks if something unexpected is at location."), SHARE(
            "-share=player1[,player2...]", "Share a lookup result with another player."), PASTE(
            "Share your results with a pastebin service and return the link");

    private final String description;
    private final String permission;
    private String usage;

    public String getDescription() {
        return description;
    }

    public String getUsage() {
        if( usage.isEmpty() ) {
            usage = "-" + this.name().toLowerCase();
        }
        return usage;
    }

    public String getPermission() {
        return permission;
    }

    public boolean hasPermission(Permissible permissible) {
        return permissible.hasPermission(permission);
    }

    /**
     * Defaults {@link #usage} to -(flagname)
     * 
     * @param description
     */
    private Flag(String description) {
        this( "", description ); // We can't use this.name() in a constructor so
                                 // we defer it to the getUsage.
    }

    private Flag(String usage, String description) {
        this.usage = usage;
        this.description = description;
        this.permission = "prism.parameters.flag." + name().toLowerCase().replace('_', '-');
    }
}