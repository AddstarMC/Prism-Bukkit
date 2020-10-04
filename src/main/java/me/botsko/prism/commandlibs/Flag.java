package me.botsko.prism.commandlibs;

import me.botsko.prism.Il8nHelper;
import org.bukkit.permissions.Permissible;

public enum Flag {
    DRAIN(Il8nHelper.getRawMessage("flag-drain")),
    DRAIN_LAVA(Il8nHelper.getRawMessage("flag-drain-lava")),
    DRAIN_WATER(Il8nHelper.getRawMessage("flag-drain-water")),
    EXTENDED(Il8nHelper.getRawMessage("flag-extended")),
    NO_EXT(Il8nHelper.getRawMessage("flag-no-extinguish")),
    NO_ITEMCLEAR(Il8nHelper.getRawMessage("flag-no-item-clear")),
    PER_PAGE("-per-page=#", Il8nHelper.getRawMessage("flag-result-per-page")),
    NO_GROUP(Il8nHelper.getRawMessage("flag-no-group")),
    OVERWRITE(Il8nHelper.getRawMessage("flag-overwrite")),
    SHARE("-share=player1[,player2...]", Il8nHelper.getRawMessage("flag-share-result")),
    PASTE(Il8nHelper.getRawMessage("flag-paste")),
    NO_PHYS(Il8nHelper.getRawMessage("flag-no-physics"));

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