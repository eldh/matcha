/*
 * Tests demonstrating mocking patterns
 */

let run = () => {
  Test.group("Mocking", () => {
    Test.run("Mock.fn returns values in sequence", () => {
      let mock = Test.Mock.fn([|"a", "b", "c"|]);
      Test.assertEqualStr(mock.call(), "a", "first call");
      Test.assertEqualStr(mock.call(), "b", "second call");
      Test.assertEqualStr(mock.call(), "c", "third call");
      Test.assertEqualStr(mock.call(), "c", "repeats last value");
    });

    Test.run("Mock.fn tracks call count", () => {
      let mock = Test.Mock.fn([|1|]);
      Test.assertEqual(mock.callCount(), 0, "no calls yet");
      let _ = mock.call();
      Test.assertEqual(mock.callCount(), 1, "one call");
      let _ = mock.call();
      Test.assertEqual(mock.callCount(), 2, "two calls");
    });

    Test.run("Mock.fnWithArg captures arguments", () => {
      let mock = Test.Mock.fnWithArg([|"response"|]);
      let _ = mock.call("request1");
      let _ = mock.call("request2");
      Test.assertEqual(mock.calls(), ["request1", "request2"], "captured args");
    });

    Test.run("Mock.const always returns same value", () => {
      let mock = Test.Mock.const(42);
      Test.assertEqual(mock.call(), 42, "first");
      Test.assertEqual(mock.call(), 42, "second");
      Test.assertEqual(mock.call(), 42, "third");
    });

    Test.run("Mock.custom uses provided function", () => {
      let mock = Test.Mock.custom((x: int) => x * 2);
      Test.assertEqual(mock.call(5), 10, "doubles input");
      Test.assertEqual(mock.call(7), 14, "doubles again");
      Test.assertEqual(mock.calls(), [5, 7], "tracked calls");
    });

    Test.run("Mock.reset clears state", () => {
      let mock = Test.Mock.fn([|"a", "b"|]);
      let _ = mock.call();
      let _ = mock.call();
      mock.reset();
      Test.assertEqual(mock.callCount(), 0, "count reset");
      Test.assertEqualStr(mock.call(), "a", "starts from beginning");
    });

    Test.run("FakeTime tracks time", () => {
      let time = Test.FakeTime.create(1000.0);
      Test.assertEqual(time.now(), 1000.0, "initial time");
      time.advance(500.0);
      Test.assertEqual(time.now(), 1500.0, "after advance");
      time.set(0.0);
      Test.assertEqual(time.now(), 0.0, "after set");
    });

    Test.run("spy tracks calls", () => {
      let s = Test.spy();
      Test.assertFalse(s.wasCalled(), "not called yet");
      s.fn("hello");
      Test.assertTrue(s.wasCalled(), "was called");
      Test.assertEqual(s.callCount(), 1, "call count");
      Test.assertEqual(s.lastCall(), Some("hello"), "last call");
    });

    Test.run("spy captures multiple calls", () => {
      let s = Test.spy();
      s.fn(1);
      s.fn(2);
      s.fn(3);
      Test.assertEqual(s.calls(), [1, 2, 3], "all calls");
      Test.assertEqual(s.lastCall(), Some(3), "last is 3");
    });
  });
};
