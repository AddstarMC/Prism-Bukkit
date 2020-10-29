package me.botsko.prism.serializers.items;

import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.reflect.TypeToken;

import java.lang.reflect.Type;


/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 30/10/2020.
 */
public class ItemStackDataDeserializer implements JsonDeserializer<ItemStackSerializer> {
    @Override
    public ItemStackSerializer deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context) throws JsonParseException {
        JsonObject jObject = json.getAsJsonObject();
        Type targetClassType;
        if(jObject.has("inventoryContents")) {
            targetClassType = new TypeToken<StorageItemStackSerializer>() { }.getType();
        } else {
            targetClassType = new TypeToken<ItemStackSerializer>() { }.getType();
        }
        return context.deserialize(json, targetClassType);
    }
}
