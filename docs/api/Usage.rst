#########
Api Usage
#########

Hooking the API
===============

This piece of code can be used to retrieve and store the api for later use.

.. code-block:: java

    static void hookPrismApi() {
        Plugin plugin = Bukkit.getPluginManager().getPlugin("Prism");
        if (plugin != null & plugin.isEnabled()) {
            PrismApi prismApi = (PrismApi) plugin;
        }
    }

Usage of the API
================

.. code-block:: java

        CommandSender sender;
        PrismParameters parameters = this.createParameters();
        parameters.addActionType("block-place");
        parameters.addPlayerName("Narimm");
        final Future<Result> result = this.performLookup(parameters, sender);
        Bukkit.getScheduler().runTaskAsynchronously(instance, new Runnable() {
            @Override
            public void run() {
                try {
                    Result done = result.get(); //Blocks until complete
                    for (me.botsko.prism.api.actions.Handler handler : done.getActionResults()) {
                        ///do something with the handler here. Remember you are Async.
                    }
                } catch (InterruptedException | ExecutionException e) {
                        //handle the exceptions
                }
            }
        });

