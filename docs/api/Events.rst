##############
Hooking Events
##############

Prism has 3 events that can be hooked to monitor Prism and its API

PrismLoadedEvent
================

.. code:: java

  public class PrismLoadedEvent extends Event {

    public PrismApi getApi() {
        return api;
    }
  }

Hooking this event allows you to get a fully loaded api if you dont want your plugin to depend on Prism and load after it.

PrismUnloadEvent
================

This event has no methods.  Hooking it can tell you when you can no longer use the Prism API


PrismRollbackEvent
==================

.. code:: java

    PrismRollBackEvent{
        /**
         * List.
         *
         * @return List BlockStateChange's
         */
        public List<BlockStateChange> getBlockStateChanges() {
            return blockStateChanges;
        }

        public ApplierResult getResult() {
            return result;
        }

        public Player getOnBehalfOf() {
            return onBehalfOf;
        }

        public PrismParameters getParameters() {
            return parameters;
        }
    }

Returns the Rollback events parameters and the player who called it and the BlockStateChanges that occurred.

PrismExtinguishEvent
====================

.. code:: java
    public class PrismExtinguishEvent extends Event {
        public ArrayList<BlockStateChange> getBlockStateChanges() {
            return blockStateChanges;
        }

        public Player onBehalfOf() {
            return onBehalfOf;
        }

        public int getRadius() {
            return radius;
        }
    }

See above for methods.


PrismDrainEvent
===============

.. code:: java
    public class PrismBlocksDrainEvent extends Event {

        public ArrayList<BlockStateChange> getBlockStateChanges() {
            return blockStateChanges;
        }

        public Player onBehalfOf() {
            return onBehalfOf;
        }

        public int getRadius() {
            return radius;
        }
    }

See above for methods.

