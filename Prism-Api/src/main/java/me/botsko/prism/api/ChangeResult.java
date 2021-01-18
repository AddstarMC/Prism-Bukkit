package me.botsko.prism.api;


/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 11/01/2021.
 */
public interface ChangeResult {

    BlockStateChange getBlockStateChange();

    ChangeResultType getType();
}
