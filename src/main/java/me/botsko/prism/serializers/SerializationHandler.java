package me.botsko.prism.serializers;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import me.botsko.prism.serializers.items.ItemStackDataDeserializer;
import me.botsko.prism.serializers.items.ItemStackSerializer;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 30/10/2020.
 */
public class SerializationHandler {
    private static final Gson gson;
    static {
        GsonBuilder builder = new GsonBuilder().disableHtmlEscaping();
        builder.registerTypeAdapter(ItemStackSerializer.class,new ItemStackDataDeserializer());
        gson = builder.create();
    }
    public static Gson gson() {
        return gson;
    }
}
