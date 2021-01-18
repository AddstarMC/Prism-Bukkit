package me.botsko.prism.parameters;

import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.api.actions.MatchRule;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.EntityType;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

public class EntityParameter extends SimplePrismParameterHandler {

    @Override
    protected List<String> tabComplete(String alias, String partialParameter, CommandSender sender) {
        List<String> result = new ArrayList<>();
        for (EntityType ent : EntityType.values()) {
            if (ent.name().startsWith(partialParameter)) {
                result.add(ent.name());
            }
        }
        return result;
    }

    public EntityParameter() {
        super("Entity", Pattern.compile("[~|!]?[\\w,]+"), "e");
    }

    @Override
    public void process(QueryParameters query, String alias, String input, CommandSender sender) {
        MatchRule match = MatchRule.INCLUDE;
        if (input.startsWith("!")) {
            match = MatchRule.EXCLUDE;
        }
        final String[] entityNames = input.split(",");
        if (entityNames.length > 0) {
            for (final String entityName : entityNames) {
                query.addEntity(entityName.replace("!", ""), match);
            }
        }
    }
}