package me.botsko.prism.serializers.items;

import me.botsko.prism.serializers.SerializationHandler;
import org.junit.Assert;
import org.junit.Test;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 2/11/2020.
 */
public class ItemStackSerializerTest {

    @Test
    public void testDeserialize() {
        ItemStackSerializer serializer = new ItemStackSerializer();
        String expected = "{\"objectType\":\"ItemStackSerializer\",\"amt\":1,\"name\":\"SOME_DIRT\",\"color\":0,\"slot\":\"-1\",\"hasFlicker\":false,\"hasTrail\":false,\"durability\":0,\"material\":\"DIRT\"}";
        serializer.amt = 1;
        serializer.material = "DIRT";
        serializer.name = "SOME_DIRT";
        String data = SerializationHandler.gson().toJson(serializer);
        Assert.assertEquals(expected,data);
        ItemStackSerializer out = SerializationHandler.gson().fromJson(expected,ItemStackSerializer.class);
        Assert.assertEquals(serializer.amt,out.amt);
        Assert.assertEquals(serializer.material,out.material);
        StorageItemStackSerializer storage = new StorageItemStackSerializer();
        storage.amt = 1;
        storage.material = "SHULKER";
        storage.inventoryContents.add(serializer);
        String storageData = SerializationHandler.gson().toJson(storage,ItemStackSerializer.class);
        Object storageNew = SerializationHandler.gson().fromJson(storageData,ItemStackSerializer.class);
        Assert.assertTrue(storageNew instanceof StorageItemStackSerializer);
        Assert.assertEquals(1,((StorageItemStackSerializer) storageNew).inventoryContents.size());
        Assert.assertEquals("DIRT",((StorageItemStackSerializer) storageNew).inventoryContents.get(0).material);
        String oldItem = "{\"amt\":1,\"name\":\"SOME_DIRT\",\"color\":0,\"slot\":\"-1\",\"hasFlicker\":false,\"hasTrail\":false,\"durability\":0,\"material\":\"SHULKER\"}";
        ItemStackSerializer oldOut = SerializationHandler.gson().fromJson(oldItem,ItemStackSerializer.class);
        Assert.assertEquals(storage.material,oldOut.material);
        TestClass test = new TestClass();
        String testString = SerializationHandler.gson().toJson(test);
        TestClass obj = SerializationHandler.gson().fromJson(testString,TestClass.class);
        Assert.assertEquals(obj.storage.inventoryContents.size(),test.storage.inventoryContents.size());


    }
}