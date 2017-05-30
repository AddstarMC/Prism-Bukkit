package me.botsko.prism.utils;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.List;

import com.helion3.pste.api.Paste;
import com.helion3.pste.api.PsteApi;
import com.helion3.pste.api.Results;

import com.google.common.base.CaseFormat;

import org.bukkit.Bukkit;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.serialization.ConfigurationSerializable;
import org.bukkit.entity.Entity;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.Player;

import net.md_5.bungee.api.ChatColor;
import net.md_5.bungee.api.chat.BaseComponent;
import net.md_5.bungee.api.chat.ClickEvent;
import net.md_5.bungee.api.chat.HoverEvent;
import net.md_5.bungee.api.chat.TextComponent;

import me.botsko.prism.Prism;
import me.botsko.prism.appliers.PrismProcessType;
import com.helion3.prism.libs.elixr.TypeUtils;
import org.bukkit.inventory.meta.ItemMeta;
import org.bukkit.util.io.BukkitObjectInputStream;
import org.bukkit.util.io.BukkitObjectOutputStream;
import org.yaml.snakeyaml.external.biz.base64Coder.Base64Coder;

public class MiscUtils {

    /**
     * Placing this here so it's easier to share the logic
     * 
     * @param player
     * @param desiredRadius
     * @param processType
     * @param config
     * @return
     */
    public static int clampRadius(Player player, int desiredRadius, PrismProcessType processType,
            FileConfiguration config) {

        if( desiredRadius <= 0 ) { return config.getInt( "prism.near.default-radius" ); }

        // Safety checks for max lookup radius
        int max_lookup_radius = config.getInt( "prism.queries.max-lookup-radius" );
        if( max_lookup_radius <= 0 ) {
            max_lookup_radius = 5;
            Prism.log( "Max lookup radius may not be lower than one. Using safe inputue of five." );
        }

        // Safety checks for max applier radius
        int max_applier_radius = config.getInt( "prism.queries.max-applier-radius" );
        if( max_applier_radius <= 0 ) {
            max_applier_radius = 5;
            Prism.log( "Max applier radius may not be lower than one. Using safe inputue of five." );
        }

        // Does the radius exceed the configured max?
        if( processType.equals( PrismProcessType.LOOKUP ) && desiredRadius > max_lookup_radius ) {
            // If player does not have permission to override the max
            if( player != null && !player.hasPermission( "prism.override-max-lookup-radius" ) ) { return max_lookup_radius; }
            // Otherwise non-player
            return desiredRadius;
        } else if( !processType.equals( PrismProcessType.LOOKUP ) && desiredRadius > max_applier_radius ) {
            // If player does not have permission to override the max
            if( player != null && !player.hasPermission( "prism.override-max-applier-radius" ) ) { return max_applier_radius; }
            // Otherwise non-player
            return desiredRadius;
        } else {
            // Otherwise, the radius is valid and is not exceeding max
            return desiredRadius;
        }
    }

    /**
     * 
     * @param prism
     * @param results
     * @return
     */
    public static String paste_results(Prism prism, String results) {

        final String prismWebUrl = "https://pste.me/";

        if( !prism.getConfig().getBoolean( "prism.paste.enable" ) ) { return Prism.messenger
                .playerError( "PSTE.me paste bin support is currently disabled by config." ); }

        final String apiUsername = prism.getConfig().getString( "prism.paste.username" );
        final String apiKey = prism.getConfig().getString( "prism.paste.api-key" );

        if( !apiKey.matches( "[0-9a-z]+" ) ) { return Prism.messenger.playerError( "Invalid API key." ); }

        final PsteApi api = new PsteApi( apiUsername, apiKey );

        try {

            final Paste paste = new Paste();
            paste.setPaste( results );

            final Results response = api.createPaste( paste );
            return Prism.messenger.playerSuccess( "Successfully pasted results: " + prismWebUrl + "#/"
                    + response.getResults().getSlug() );

        } catch ( final Exception up ) {
            Prism.debug( up.toString() );
            return Prism.messenger.playerError( "Unable to paste results (" + ChatColor.YELLOW + up.getMessage()
                    + ChatColor.RED + ")." );
        }
    }

    public static List<String> getStartingWith(String start, Iterable<String> options, boolean caseSensitive) {
        final List<String> result = new ArrayList<>();
        if( caseSensitive ) {
            for ( final String option : options ) {
                if( option.startsWith( start ) )
                    result.add( option );
            }
        } else {
            start = start.toLowerCase();
            for ( final String option : options ) {
                if( option.toLowerCase().startsWith( start ) )
                    result.add( option );
            }
        }

        return result;
    }

    public static List<String> getStartingWith(String arg, Iterable<String> options) {
        return getStartingWith( arg, options, true );
    }

    public static void dispatchAlert(String msg, List<String> commands) {
        String colorized = TypeUtils.colorize(msg);
        String stripped = ChatColor.stripColor(colorized);
        for (String command : commands) {
            if(command.equals("examplecommand <alert>"))
                continue;
            String processedCommand = command.replace("<alert>", stripped);
            Bukkit.dispatchCommand(Bukkit.getConsoleSender(), processedCommand);
        }
    }

    public static String getEntityName(Entity entity) {
        if(entity == null)
            return "unknown";
        if(entity.getType() == EntityType.PLAYER)
            return ((Player)entity).getName();
        return CaseFormat.UPPER_UNDERSCORE.to(CaseFormat.UPPER_CAMEL, entity.getType().name());
    }

    public static BaseComponent getPreviousButton() {
        TextComponent textComponent = new TextComponent("[Prev]");
        textComponent.setColor(ChatColor.GRAY);
        textComponent.setHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, new TextComponent[] {
                new TextComponent("Click to view the previous page") }));
        textComponent.setClickEvent(new ClickEvent(ClickEvent.Action.RUN_COMMAND, "/pr pg p"));
        return textComponent;
    }

    public static BaseComponent getNextButton() {
        TextComponent textComponent = new TextComponent("        ");
        textComponent.setColor(ChatColor.GRAY);
        textComponent.addExtra(getNextButtonComponent());
        return textComponent;
    }

    private static BaseComponent getNextButtonComponent() {
        TextComponent textComponent = new TextComponent("[Next]");
        textComponent.setHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, new TextComponent[] {
                new TextComponent("Click to view the next page") }));
        textComponent.setClickEvent(new ClickEvent(ClickEvent.Action.RUN_COMMAND, "/pr pg n"));
        return textComponent;
    }

    public static BaseComponent getPrevNextButtons() {
        TextComponent textComponent = new TextComponent();
        textComponent.setColor(ChatColor.GRAY);
        textComponent.addExtra(getPreviousButton());
        textComponent.addExtra(getNextButtonComponent());
        return textComponent;
    }

    /**
     * Serializes given Bukkit object to a base64 string, for data storage. Uses
     * {@link BukkitObjectOutputStream}, so data of items should serialize properly. If given null,
     * will just return null silently.
     *
     * @param obj ConfigurationSerializable to serialize to a base64 string
     * @return Base64 representation of the given object, or null if it failed to serialize
     */
    public static String serializeToBase64(ConfigurationSerializable obj)
    {
        if (obj == null)
            return null;

        // Won't use try-with-resources here; ByteArrayOutputStream requires another try/catch
        // This generates garbage, but unfortunately BukkitObjectOutputStream.reset() is broken
        ByteArrayOutputStream outputStream;
        BukkitObjectOutputStream dataObject;

        try
        {
            outputStream = new ByteArrayOutputStream();
            dataObject   = new BukkitObjectOutputStream(outputStream);
            dataObject.writeObject(obj);
            dataObject.close();
            outputStream.close();

            return Base64Coder.encodeLines( outputStream.toByteArray() );
        }
        catch (Exception e)
        {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * Deserializes given base64 string to Bukkit object, for data restore. Uses
     * {@link BukkitObjectInputStream}, so data of items should deserialize properly. If given null,
     * will just return null silently.
     *
     * @param base64 Base64 string to deserialize into a ConfigurationSerializable
     * @return A ConfigurationSerializable object, or null if it failed to deserialize
     */
    @SuppressWarnings("unchecked")
    public static <T extends ConfigurationSerializable> T deserializeFromBase64(String base64)
    {
        if (base64 == null)
            return null;

        // Won't use try-with-resources here; too noisy
        ByteArrayInputStream    inputStream;
        BukkitObjectInputStream dataInput;

        try
        {
            byte[] decoded = Base64Coder.decodeLines(base64);

            inputStream = new ByteArrayInputStream(decoded);
            dataInput   = new BukkitObjectInputStream(inputStream);

            return (T) dataInput.readObject();
        }
        catch (Exception e)
        {
            e.printStackTrace();
            return null;
        }
    }
}