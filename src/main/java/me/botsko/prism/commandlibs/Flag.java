package me.botsko.prism.commandlibs;

import me.botsko.prism.Il8n;
import org.bukkit.permissions.Permissible;

public enum Flag {
    DRAIN(Il8n.getRawMessage("flag-drain")),
    DRAIN_LAVA(Il8n.getRawMessage("flag-drain-lava")),
    DRAIN_WATER(Il8n.getRawMessage("flag-drain-water")),
    EXTENDED(Il8n.getRawMessage("flag-extended")),
    NO_EXT(Il8n.getRawMessage("flag-no-extinguish")),
    NO_ITEMCLEAR(Il8n.getRawMessage("flag-no-item-clear")),
    PER_PAGE("-per-page=#", Il8n.getRawMessage("flag-result-per-page")),
    NO_GROUP(Il8n.getRawMessage("flag-no-group")),
    OVERWRITE(Il8n.getRawMessage("flag-overwrite")),
    SHARE("-share=player1[,player2...]", Il8n.getRawMessage("flag-share-result")),
    PASTE(Il8n.getRawMessage("flag-paste")),
    NO_PHYS(Il8n.getRawMessage("flag-no-physics"));

    private final String description;
    private final String permission;
    private String usage;

    /**
     * Defaults {@link #usage} to -(flagname).
     *
     * @param description string
     */
    Flag(String description) {
        this("", description); // We can't use this.name() in a constructor so
        // we defer it to the getUsage.
    }

    Flag(String usage, String description) {
        this.usage = usage;
        this.description = description;
        this.permission = "prism.parameters.flag." + name().toLowerCase().replace('_', '-');
    }

    public String getDescription() {
        return description;
    }

    /**
     * Get usage for flag.
     * @return String
     */
    public String getUsage() {
        if (usage.isEmpty()) {
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
}