package me.botsko.prism;

import org.bukkit.plugin.Plugin;
import org.bukkit.scheduler.BukkitScheduler;
import org.bukkit.scheduler.BukkitTask;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.logging.Level;

/**
 * Created for Prism.  This class handles futures checking them for completion asynchronously and then running the
 * supplied task if completed.
 *
 * @author Narimm on 15/02/2021
 * @since 2.1.8
 */
public class TaskManager {
    public boolean active;
    private final BukkitScheduler schedule;
    private BukkitTask core;
    private final ExecutorService service;
    private final Plugin plugin;
    private final BlockingQueue<FunctionalBukkitTask<?>> tasks = new LinkedBlockingQueue<>();

    /**
     * This class handles futures checking them for completion asynchronously and then running the supplied task
     * if completed.
     *
     * @param schedule BukkitScheduler for synchronous execution
     * @param plugin   the plugin.
     */
    public TaskManager(BukkitScheduler schedule, Plugin plugin) {
        this.schedule = schedule;
        this.plugin = plugin;
        service = Executors.newCachedThreadPool();
        active = true;
    }

    /**
     * Adds a task in a non blocking fashion.
     *
     * @param future The future to check
     * @param task   the Task to run once the future is complete
     * @param async  run the task async or on main thread
     * @param <T>    The object returned by the future and accepted by the task.
     */
    public <T> void addTask(CompletableFuture<T> future, Consumer<T> task, boolean async) throws InterruptedException {
        if (active) {
            tasks.put(new FunctionalBukkitTask<>(future, task, async));
        } else {
            throw new RuntimeException("TaskManager inactive.");
        }

    }

    /**
     * Shutdown the task Manager.
     * @return True of all tasks are finished.
     */
    public boolean shutdown() {
        active = false;
        try {
            if (service.awaitTermination(100,TimeUnit.MILLISECONDS)) {
                core.cancel();
                tasks.clear();
                return true;
            } else {
                PrismLogHandler.warn("Task manager could not stop all executing tasks...and timed out while waiting");
                return fastShutdown();
            }
        } catch (InterruptedException e) {
            PrismLogHandler.warn(Level.WARNING,"Task manager threw an error while terminating",e);
            return fastShutdown();
        }
    }

    private boolean fastShutdown() {
        int failed = service.shutdownNow().size();
        core.cancel();
        tasks.clear();
        if (failed > 0) {
            PrismLogHandler.warn("Task manager: " + failed + " tasks where force stopped.");
            return false;
        }
        return  true;
    }

    /**
     * Start the task manager.
     */
    public void run() {
        core = schedule.runTaskAsynchronously(plugin, () -> {
            while (active) {
                try {
                    FunctionalBukkitTask<?> next = tasks.poll(100, TimeUnit.MILLISECONDS);
                    if (next != null) {
                        if (next.isDone()) {
                            next.complete();
                        } else {
                            tasks.put(next);
                        }
                    }
                } catch (InterruptedException e) {
                    e.printStackTrace();
                    active = false;
                }
            }
            PrismLogHandler.log("Task manager is stopping...");
        });
    }

    public class FunctionalBukkitTask<T> {
        private final CompletableFuture<T> future;
        private final Consumer<T> onCompletion;
        private final boolean async;
        private T result;

        /**
         * A task that will call the consumer on completion running it sync if needed.
         */
        public FunctionalBukkitTask(CompletableFuture<T> future, Consumer<T> function, boolean async) {
            this.future = future;
            this.onCompletion = function;
            this.async = async;
        }

        public boolean isDone() {
            return future.isDone();
        }

        /**
         * Completes the task once the future is done.
         */
        public void complete() {
            try {
                result = future.get();
                if (async) {
                    service.execute(() -> onCompletion.accept(result));
                } else {
                    schedule.runTask(plugin, () -> onCompletion.accept(result));
                }
            } catch (InterruptedException | ExecutionException e) {
                e.printStackTrace();
            }
        }
    }
}


