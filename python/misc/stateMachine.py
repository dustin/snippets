#!/usr/bin/env python
"""

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 70057323-43E7-11D9-9979-000393CFE6B8

import exceptions
import unittest

# Abstract finite state machine prototype

class StateRv(object):
    """Return values for state machines."""

    def __init__(self, state, prefix=None, postfix=None):
        """Instantiate a state rv with a state and an optional prefix and
        postfix state machine to adjust the values.

        A prefix state machine will be run beginning in the init state, and the
        provided state will be used to continue the current machine after the
        prefixed machine completes."""
        self.state=state
        self.prefix=prefix
        self.postfix=postfix

class StateMachine(object):
    """State machine interface."""

    STATE_INIT = -1
    STATE_DONE = -2

    def runMachine(self, context, state):
        """This method should do whatever it needs to do in the given state and
           return the new state."""
        raise NotImplementedError()

class TestMachine(StateMachine):
    """A test state machine that does nothing but push through a sequence of
    states."""

    def __init__(self):
        self.stateMap={StateMachine.STATE_INIT: StateRv(1),
            1: StateRv(2),
            2: StateRv(3),
            3: StateRv(StateMachine.STATE_DONE)}

    def runMachine(self, context, state):
        return self.stateMap[state]

class TestPrefixingMachine(TestMachine):
    """A test state machine that runs through a sequence of states, but
       prefixes a new state machine in the middle."""

    def __init__(self):
        self.stateMap={StateMachine.STATE_INIT: StateRv(1),
            1: StateRv(2),
            2: StateRv(3, prefix=TestMachine()),
            3: StateRv(4),
            4: StateRv(StateMachine.STATE_DONE)}

class TestPostfixingMachine(TestMachine):
    """A test state machine that runs through a sequence of states, but
       prefixes a new state machine in the middle."""

    def __init__(self):
        self.stateMap={StateMachine.STATE_INIT: StateRv(1),
            1: StateRv(2),
            2: StateRv(3, prefix=None, postfix=TestMachine()),
            3: StateRv(4),
            4: StateRv(5),
            5: StateRv(StateMachine.STATE_DONE)}

#
# Client stuff
#

class NoStateMachineException(exceptions.Exception):
    """Exception thrown when we try to pass state to a state machine, but don't
    have one."""

class StateMachineList(object):
    """A list of state machines to run to completion in sequence.  This is the
    state machine abstraction that will be held for the client."""

    def __init__(self, context):
        self.context=context
        self.machines=[]
        self.stateStack=[]
        self.state=StateMachine.STATE_INIT

    def addMachine(self, machine):
        """Add a machine to be processed in the future."""
        self.machines.append(machine)

    def runMachine(self):
        """Run the current state machine.  Return True if there's more to do."""
        if len(self.machines) == 0:
            raise NoStateMachineException()

        rv = True

        newState = self.machines[0].runMachine(self.context, self.state)

        # Check for a postfix
        if newState.postfix is not None:
            self.addMachine(newState.postfix)

        # Check for a prefix
        if newState.prefix is not None:
            # We're prefixing a state machine to the list.  Preserve the
            # starting state for the current machine and add the new state
            self.stateStack.append(newState.state)
            self.machines.insert(0, newState.prefix)
            # Set the state to init for the new state machine item
            self.state=StateMachine.STATE_INIT
        elif newState.state == StateMachine.STATE_DONE:
            # Finished a state machine, pop it off the list.
            self.machines.pop(0)
            # If it's empty, we're done
            if len(self.machines) == 0:
                rv = False
            # Check for a stored state, otherwise set it to init
            if len(self.stateStack) > 0:
                self.state=self.stateStack.pop()
            else:
                self.state=StateMachine.STATE_INIT
        else:
            self.state=newState.state

        return rv

#
## Testing
#
class StateMachineTest(unittest.TestCase):

    def assertState(self, v, st):
        """Assert the given state return object's state equals v"""
        self.assertEquals(v, st.state)

    def testPlainMachine(self):
        """Plain state machine test."""
        tm=TestMachine()
        self.assertState(1, tm.runMachine(None, StateMachine.STATE_INIT))
        self.assertState(2, tm.runMachine(None, 1))
        self.assertState(3, tm.runMachine(None, 2))
        self.assertState(StateMachine.STATE_DONE, tm.runMachine(None, 3))

    def assertStateSequence(self, sm, seq):
        """Validate that the execution of a state machine yields a specific
           sequence of states."""
        # Check the initial state
        self.assertEquals(seq[0], sm.state)
        # Validate the state sequence
        for s in seq[1:]:
            self.failUnless(sm.runMachine(), "State machine run")
            # print "Checking", s, "in", sm.state, "from", sm.machines[0]
            self.assertEquals(s, sm.state)

        # A final run which should mark the end
        self.failIf(sm.runMachine(), "State machine should end")

        try:
            sm.runMachine()
            self.fail("State machine should be complete")
        except NoStateMachineException:
            pass

    def testStateList(self):
        """Test a hand-coded sequence of a state machine."""
        slist=StateMachineList(None)

        # Add two TestMachines
        slist.addMachine(TestMachine())
        slist.addMachine(TestMachine())

        self.assertEquals(StateMachine.STATE_INIT, slist.state)
        self.failUnless(slist.runMachine(), "State machine run")
        self.assertEquals(1, slist.state)
        self.failUnless(slist.runMachine(), "State machine run")
        self.assertEquals(2, slist.state)
        self.failUnless(slist.runMachine(), "State machine run")
        self.assertEquals(3, slist.state)
        self.failUnless(slist.runMachine(), "State machine run")
        self.assertEquals(StateMachine.STATE_INIT, slist.state)
        self.failUnless(slist.runMachine(), "State machine run")
        self.assertEquals(1, slist.state)
        self.failUnless(slist.runMachine(), "State machine run")
        self.assertEquals(2, slist.state)
        self.failUnless(slist.runMachine(), "State machine run")
        self.assertEquals(3, slist.state)
        self.failIf(slist.runMachine(), "State machine should end")

        try:
            slist.runMachine()
            self.fail("State machine should be complete")
        except NoStateMachineException:
            pass

    def testStateList2(self):
        """Same as testStateList, but using assertStateSequence"""
        slist=StateMachineList(None)

        # Add two TestMachines
        slist.addMachine(TestMachine())
        slist.addMachine(TestMachine())

        self.assertStateSequence(slist, [StateMachine.STATE_INIT,
            1, 2, 3, StateMachine.STATE_INIT, 1, 2, 3])

    def testPrefixing(self):
        """Test a state machine list including a state machine that will prefix
        another state machine in the list"""
        slist=StateMachineList(None)

        # Add two TestMachines
        slist.addMachine(TestMachine())
        slist.addMachine(TestPrefixingMachine())
        slist.addMachine(TestMachine())

        self.assertStateSequence(slist, [
            # TestMachine
            StateMachine.STATE_INIT, 1, 2, 3,
            # TestPrefixingMachine - 2, then ...
            StateMachine.STATE_INIT, 1, 2,
            # ... a TestMachine that got prefixed
            StateMachine.STATE_INIT, 1, 2, 3,
            # follwed by the continuation of the TestPrefixingMachine
            3, 4,
            # And then another TestMachine as defined above
            StateMachine.STATE_INIT, 1, 2, 3
            ])

    def testPrefixing(self):
        """Test a state machine list including a state machine that will prefix
        another state machine in the list"""
        slist=StateMachineList(None)

        # Add two TestMachines
        slist.addMachine(TestMachine())
        slist.addMachine(TestPrefixingMachine())
        slist.addMachine(TestPostfixingMachine())
        slist.addMachine(TestMachine())

        self.assertStateSequence(slist, [
            # TestMachine
            StateMachine.STATE_INIT, 1, 2, 3,
            # TestPrefixingMachine - 2, then ...
            StateMachine.STATE_INIT, 1, 2,
            # ... a TestMachine that got prefixed
            StateMachine.STATE_INIT, 1, 2, 3,
            # follwed by the continuation of the TestPrefixingMachine
            3, 4,
            # then the postfixer as defined above
            StateMachine.STATE_INIT, 1, 2, 3, 4, 5,
            # then the TestMachine that was brought in by the postfixer
            StateMachine.STATE_INIT, 1, 2, 3,
            # And then the TestMachine that was brought in by the postfixer
            StateMachine.STATE_INIT, 1, 2, 3
            ])

if __name__ == '__main__':
    unittest.main()
