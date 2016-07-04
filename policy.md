
# Principles

1. Correctness

Code should not leak resources or corrupt internal invariants.
Also it should not unexpectedly swallow exceptions.

2. Ease

Writing exception-safe code should be as easy as it is possible.
Though exception handling is complex by nature, so ease should not
mitigate correctness.

3. Uniformness

Where possible, handling async exceptions should be the same as handling
sync exceptions. It means that policies and best practices should
encourage techniques that work with both types of exceptions.

When async exceptions require special care, it is desired to indicate that
explicitly in source code.

4. Backward compatibility

It is important to minimize code breakage as a result of applying this
policy. But preserving backward compatibility should not mitigate
correctness.


# Approach

We adopt the widely used [approach](http://www.boost.org/community/exception_safety.html)
to exception safety.


# Policy

* IO actions that provides `nothrow` exception safety level should be documented as such.
If `nothrow` could be guaranteed only in specific cases, it should be documented too.

Rationale: Right now this information is hard to find (Ease). But it is necessary to
write correct code.

* IO actions that are not interruptible should be documented as such. An action is
assumed to be interruptible by default.

Rationale: Right now this information is hard to find (Ease). But it is necessary to
write correct code.

* Cleanup and acquire actions should be documented as such. If an action is not
documented to be a cleanup (acquire), then it should not be used as a cleanup (acquire).

Rationale: Cleanup, acquire and regular actions are not interchangeable in general case.
So they should be specially crafted for their specific use case. (Correctness)

* Cleanup should not leak resources or corrupt internal invariants in case of failure.
This should be true both for sync and async exceptions.

Rationale: Correctness. Uniformness. Also, such definition is backward compatible --
cleanup actions confirming this policy can be used with old code, written before
the policy was accepted. If we allow cleanups to leak in case of async exception,
then they will not be safe to use with old code.

* Cleanups should be prepared to be executed under `uninterruptibleMask_`, but should
not rely on that.

Rationale: In case of `bracket before after thing` there is no way to handle
async exception thrown from `after` when `thing` already failed (otherwise
one of the exceptions will be unexpectedly swallowed, aka "double-throw issue")
, so `bracket` should disable async exceptions using `uninterruptibleMask`
in that case. (Correctness) But cleanup (or any other code) should not rely
on that because it could be used in old code. (Backward Compatibility)

* Cleanups should not block (use interruptible action) unless it is strictly necessary to release resources.
(In the later case it should use `uninterruptibleMask` according to the previous policy)

Rationale: Action that block may become not responsive when executed
under `uninterruptibleMask`, so such cases should be minimized.

* Generic components that use cleanups (e.g. `bracket`) should be prepared to
handle interruptible cleanups.

Rationale: Backward Compatibility.

# Migration Plan

The policy is specially crafted to make sure the old and new code are (mostly)
compatible. E.g. old cleanups can be used with new `bracket`, and new cleanups
can be used with old `bracket`.

Migration occurs in the next steps:

* Accept the policy. All new code is required to confirm to it. No code is broken.

Rationale: Let community adopt the policy and fix any found breakage in existing code.

* Add `uninterruptibleMask_` in `bracket`, `catch`, `finally`,
but only when the main action already failed. Minimal amount of code breakage.

Rationale: This step establishes correctness. Some, but not all old code will be broken.
Give community additional time to fix any breakage.

* Add `uninterruptibleMask_` in `bracket`, `catch`, `finally` both when the main
action fails and when it doesn't.

Rationale: This step breaks more code, but we expect most of it will already be fixed.

* Cleanup the policy to remove backward compatibility notices.

Rationale: Some of the policies becomes obsolete, so they should be revised.
E.g. we may allow cleanups to rely on `uninterruptibleMask` in `bracket` for ease
of use.
