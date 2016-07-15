## Maybe use dirty schedulers

We could build the old `re2_nif.so` plus `re2_nif_ds.so`, and load the dirty-schedulers variant if a certain re2.app env var was set before on_load is triggered.

Steve Vinoski's comment from [#9](https://github.com/tuncer/re2/issues/9):

Dirty schedulers are beneficial when you have NIF tasks that regularly run too long on a normal scheduler thread &mdash; where "too long" means longer than 1-2ms &mdash; and there's no natural or easy way to break the work into chunks. One way to determine if your NIF workloads run too long on a normal scheduler is to use [`erlang:system_monitor/2`](http://erlang.org/doc/man/erlang.html#system_monitor-2) with the `{long_schedule, Time}` option, where you set `Time` to 2 (milliseconds).

In Erlang 17 and 18 dirty schedulers are labeled as an experimental feature, which means they're off by default and so if you don't specifically configure and build your system with them enabled, they won't be available. We hope to remove the experimental label for Erlang 19, but until then, you can conditionally compile code using dirty schedulers under the `ERL_NIF_DIRTY_SCHEDULER_SUPPORT` preprocessor macro, which is defined only when the system supports dirty schedulers. To check at runtime whether dirty schedulers are enabled, call [`enif_system_info()`](http://erlang.org/doc/man/erl_nif.html#enif_system_info) and then verify that the `smp_support` and `dirty_scheduler_support` fields in the resulting `ErlNifSysInfo` struct are non-zero (and yes, you're right you shouldn't have to check both fields, but for now you do).

The dirty scheduler API is relatively simple, since all it requires is some preprocessor macros, `ERL_NIF_DIRTY_JOB_CPU_BOUND` and `ERL_NIF_DIRTY_JOB_IO_BOUND`, that developers use to tell the Erlang runtime where to run a NIF. The API has been stable for some time, ever since I introduced [`enif_schedule_nif()`](http://erlang.org/doc/man/erl_nif.html#enif_schedule_nif) in 17.3. The `enif_schedule_nif()` function allows you to decide at runtime to schedule NIF work on a normal scheduler or on a dirty scheduler. These macros can also be specified in the `flags` field of an [`ErlNifFunc` instance](http://erlang.org/doc/man/erl_nif.html#ErlNifFunc) so that a particular NIF is always dispatched to a dirty scheduler.

The bottom line is that if your NIF tasks regularly run too long and cause `long_schedule` events to regularly occur, you have three options:

 * *Break the NIF into multiple functions and orchestrate their calls from Erlang.* The idea here is to hide NIF calls within a regular Erlang function, where each NIF call is short enough to avoid `long_schedule` problems and the Erlang code ties them all together. This keeps the normal Erlang schedulers happy because the NIFs return to Erlang frequently.

 * *Break a long-running NIF into chunks of work that can be scheduled with `enif_schedule_nif()`.* With this approach, a NIF runs for a time that avoids `long_schedule` problems and if it still has work to do, essentially reschedules itself on a normal scheduler by calling `enif_schedule_nif()`. This keeps the normal Erlang schedulers happy because the NIF yields the scheduler thread whenever it calls `enif_schedule_nif()`, but this approach requires a way to break the work up so it can be done in chunks.

 * *Run the work on a dirty scheduler.* If the work can't be easily broken up with either of the previous two approaches, then it can run on a dirty scheduler. The [overhead of switching to and from a dirty scheduler is quite tiny](https://medium.com/@jlouis666/erlang-dirty-scheduler-overhead-6e1219dcc7#.m8kzikb64) and will almost certainly be in the noise compared to the work the task performs. One thing to keep in mind is that dirty scheduler thread pools, one meant for CPU-bound jobs and the other for I/O-bound jobs, are shared resources, so if lots of processes are using them, jobs will queue up for them. If you have tasks that are just going to sit and block on a dirty scheduler, you can help ensure fairness for other tasks by cooperatively yielding a dirty scheduler by calling `enif_schedule_nif()` to reschedule yourself for future execution either on a dirty scheduler or back on a regular scheduler if the future work is such that it can run there without triggering `long_schedule` problems.