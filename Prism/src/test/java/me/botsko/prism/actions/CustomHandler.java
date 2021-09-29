package me.botsko.prism.actions;

import be.seeseemelk.mockbukkit.UnimplementedOperationException;

/**
 * An example of a custom handler.
 * Created for Prism.
 *
 * @author Narimm on 18/08/2021
 * @since 2.1.8
 */
public class CustomHandler extends GenericAction {

    @Override
    public boolean hasExtraData() {
        return false;
    }

    @Override
    public String serialize() {
        return null;
    }

    @Override
    public void deserialize(String data) {
        throw new UnimplementedOperationException("Not Implemented");
    }

    @Override
    public String getNiceName() {
        return null;
    }

}
