package me.botsko.prism.commandlibs;

import java.util.List;

public interface SubHandler {
    void handle(CallInfo call);

    List<String> handleComplete(CallInfo call);
}