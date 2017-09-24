erl_vcron
=========

Given a [calendar:datetime()](http://erlang.org/doc/man/calendar.html#type-datetime)
and a [Vixie Cron-like expression](https://en.wikipedia.org/wiki/Cron),
returns `true` if the expression covers the datetime().

# Build
Just run `make`.

# Examples
```
1> erl_vcron:applies({{2017, 07, 10}, {4, 15, 0}}, "*/5 2-4 10,15 7 1").
```

There are many more examples in the [eunit tests](https://github.com/marcelog/erl_vcron/blob/master/test/erl_vcron_test.erl).

## License
The source code is released under Apache 2 License.

Check [LICENSE](https://github.com/marcelog/erl_vcron/blob/master/LICENSE) file for more information.
