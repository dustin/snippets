#!/usr/bin/env python
"""

Copyright (c) 2004  Dustin Sallings <dustin@spy.net>
"""

import exceptions
import unittest

# Abstract finite state machine prototype

class StateRv(object):
    """Return values for state machines."""

    def __init__(self, state, response, prefixes=[], postfixes=[]):
        """Instantiate a state rv with a state and optional lists of prefix
        and postfix state machines to adjust the values.

        A prefix state machine will be run beginning in the init state, and the
        provided state will be used to continue the current machine after the
        prefixed machine completes."""
        self.state=state
        self.prefixes=prefixes
        self.postfixes=postfixes
        self.response=response

class StateMachine(object):
    """State machine interface."""

    STATE_INIT = -1
    STATE_DONE = -2

    def runMachine(self, context, state, input):
        """This method should do whatever it needs to do in the given state and
           return the new state."""
        raise NotImplementedError()

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

    def addPrefixMachine(self, machine):
        """Add a machine in the beginning."""
        self.machines.insert(0, machine)

    def complete(self):
        """True if all of the state machines have been completed."""
        return (len(self.machines) == 0)

    def runMachine(self, input):
        """Run the current state machine.  Return True if there's more to do."""
        if len(self.machines) == 0:
            raise NoStateMachineException()

        newState = self.machines[0].runMachine(self.context, self.state, input)

        # Extract the response for the return value
        rv = newState.response

        # Add the postfixes
        for sm in newState.postfixes:
            self.addMachine(sm)

        # Check for prefixes
        if len(newState.prefixes) > 0:
            # We're prefixing a state machine to the list.  Preserve the
            # starting state for the current machine and add the new state.
            # The list is reversed because we want to have them run in returned
            # order
            newState.prefixes.reverse()
            self.stateStack.append(newState.state)
            self.addPrefixMachine(newState.prefixes[0])
            # For any additional state machines, add them with the init state
            for sm in newState.prefixes[1:]:
                self.stateStack.append(StateMachine.STATE_INIT)
                self.addPrefixMachine(sm)
            # Set the state to init for the new state machine item
            self.state=StateMachine.STATE_INIT
        elif newState.state == StateMachine.STATE_DONE:
            # Finished a state machine, pop it off the list.
            self.machines.pop(0)
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

# The return value everything will return
RV = "rv"
IN = "in"

class UnexpectedInputException(exceptions.Exception):
    def __init__(self, x):
        exceptions.Exception.__init__(self)
        self.x=x

    def __repr__(self):
        return "<UnexpectedInputException: " + `x` + ">"

class TestMachine(StateMachine):
    """A test state machine that does nothing but push through a sequence of
    states."""

    def __init__(self, stateMap=None):
        if stateMap is None:
            stateMap={StateMachine.STATE_INIT: StateRv(1, RV),
                1: StateRv(2, RV),
                2: StateRv(3, RV),
                3: StateRv(StateMachine.STATE_DONE, RV)}
        self.stateMap=stateMap
    
    def runMachine(self, context, state, input):
        if input != IN:
            raise UnexpectedInputException(input)
        return self.stateMap[state]

class TestPrefixingMachine(TestMachine):
    """A test state machine that runs through a sequence of states, but
       prefixes a new state machine in the middle."""

    def __init__(self):
        TestMachine.__init__(self, {StateMachine.STATE_INIT: StateRv(1, RV),
            1: StateRv(2, RV),
            2: StateRv(3, RV, prefixes=[TestMachine()]),
            3: StateRv(4, RV),
            4: StateRv(StateMachine.STATE_DONE, RV)})

class StateMachineTest(unittest.TestCase):

    def assertState(self, v, st):
        """Assert the given state return object's state equals v"""
        self.assertEquals(v, st.state)

    def testPlainMachine(self):
        """Plain state machine test."""
        tm=TestMachine()
        self.assertState(1, tm.runMachine(None, StateMachine.STATE_INIT, IN))
        self.assertState(2, tm.runMachine(None, 1, IN))
        self.assertState(3, tm.runMachine(None, 2, IN))
        self.assertState(StateMachine.STATE_DONE, tm.runMachine(None, 3, IN))

    def assertStateSequence(self, sm, seq):
        """Validate that the execution of a state machine yields a specific
           sequence of states."""
        # Check the initial state
        # Validate the state sequence
        for s in seq:
            self.assertEquals(s, sm.state)
            self.assertEquals(RV, sm.runMachine(IN))
        self.assertEquals(StateMachine.STATE_INIT, sm.state)
        self.failUnless(sm.complete())

        try:
            sm.runMachine(IN)
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
        self.assertEquals(RV, slist.runMachine(IN))
        self.assertEquals(1, slist.state)
        self.assertEquals(RV, slist.runMachine(IN))
        self.assertEquals(2, slist.state)
        self.assertEquals(RV, slist.runMachine(IN))
        self.assertEquals(3, slist.state)
        self.assertEquals(RV, slist.runMachine(IN))
        self.assertEquals(StateMachine.STATE_INIT, slist.state)
        self.assertEquals(RV, slist.runMachine(IN))
        self.assertEquals(1, slist.state)
        self.assertEquals(RV, slist.runMachine(IN))
        self.assertEquals(2, slist.state)
        self.assertEquals(RV, slist.runMachine(IN))
        self.assertEquals(3, slist.state)
        self.assertEquals(RV, slist.runMachine(IN))
        self.assertEquals(StateMachine.STATE_INIT, slist.state)
        self.failUnless(slist.complete())

        try:
            slist.runMachine(IN)
            self.fail("State machine should be complete")
        except NoStateMachineException:
            pass

    def testBadInputInList(self):
        """Test providing bad input to a state machine list."""
        slist=StateMachineList(None)
        # Add a test machine
        slist.addMachine(TestMachine())

        try:
            slist.runMachine("badinput")
            self.fail("State machine incorrectly handled bad input.")
        except UnexpectedInputException:
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

    def testPostfixing(self):
        """Test a state machine list including a state machine that will prefix
        another state machine in the list"""
        slist=StateMachineList(None)

        # Add two TestMachines
        slist.addMachine(TestMachine())
        slist.addMachine(TestPrefixingMachine())
        # Try it postfixing
        slist.addMachine(TestMachine({StateMachine.STATE_INIT: StateRv(1, RV),
            1: StateRv(2, RV),
            2: StateRv(3, RV, postfixes=[TestMachine()]),
            3: StateRv(4, RV),
            4: StateRv(5, RV),
            5: StateRv(StateMachine.STATE_DONE, RV)}))
        # And another plain test machine
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
            # then the TestMachine that was defined above
            StateMachine.STATE_INIT, 1, 2, 3,
            # And then the TestMachine that was brought in by the postfixer
            StateMachine.STATE_INIT, 1, 2, 3
            ])

    def testDoublePrefixing(self):
        """Test a state machine list including a state machine that will prefix
        two state machines in the list"""
        slist=StateMachineList(None)

        aStates={StateMachine.STATE_INIT: StateRv(10, RV),
            10: StateRv(11, RV), 11: StateRv(12, RV),
                12: StateRv(StateMachine.STATE_DONE, RV)}
        bStates={StateMachine.STATE_INIT: StateRv(20, RV),
            20: StateRv(21, RV), 21: StateRv(22, RV),
                22: StateRv(StateMachine.STATE_DONE, RV)}

        # Add two TestMachines
        slist.addMachine(TestMachine())
        slist.addMachine(TestPrefixingMachine())
        # Try it postfixing
        slist.addMachine(TestMachine({StateMachine.STATE_INIT: StateRv(1, RV),
            1: StateRv(2, RV),
            2: StateRv(3, RV,
                prefixes=[TestMachine(aStates), TestMachine(bStates)]),
            3: StateRv(4, RV),
            4: StateRv(StateMachine.STATE_DONE, RV)}))
        # And another plain test machine
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
            # Now for the weird double prefixer
            StateMachine.STATE_INIT, 1, 2,
            # First test
            StateMachine.STATE_INIT, 10, 11, 12,
            # Second test
            StateMachine.STATE_INIT, 20, 21, 22,
            # Finish the weird machine
            3, 4,
            # And one more Test machine
            StateMachine.STATE_INIT, 1, 2, 3,
            ])

if __name__ == '__main__':
    unittest.main()
