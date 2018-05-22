package me.botsko.prism.actionlibs;

import java.lang.reflect.InvocationTargetException;

import me.botsko.prism.actions.Handler;

public class HandlerFactory<H> {

    /**
	 * 
	 */
    final Class<? extends Handler> handlerClass;

    /**
     * 
     * @param handlerClass
     */
    public HandlerFactory(Class<? extends Handler> handlerClass) {
        this.handlerClass = handlerClass;
    }

    /**
     * 
     * @return
     * @throws InstantiationException
     * @throws IllegalAccessException
     * @throws SecurityException 
     * @throws NoSuchMethodException 
     * @throws InvocationTargetException 
     * @throws IllegalArgumentException 
     */
    public Handler create() throws InstantiationException, IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchMethodException, SecurityException {
    	return handlerClass.getConstructor().newInstance();
    }
}