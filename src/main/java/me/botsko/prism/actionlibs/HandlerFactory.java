package me.botsko.prism.actionlibs;

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
	 */
	public Handler create() throws InstantiationException, IllegalAccessException {
		Handler handler = handlerClass.newInstance();
        return handler;
    }
}