# Resource Pool

## Introduction
Some software resources have time and memory cost to create and reusing them can dramatically improve application performance.
Resource pooling is widely used for resource reusing in different platform and languages. This article deals with 
[Erlang Resource Pool](http://sourceforge.net/projects/erlpool) project in Sourceforge that was inspired by 
Apache Commons Pool Java library. API and main functioning principals was borrowed from there, but internal 
implementation is completely different and is using Erlang OTP design principles and Erlang concurrent model.

## Design
Resource pool consists of two containers: `Active` and `Idle`. `Active` container keeps references to
resources that are actively used by some processes. Oppositely `Idle` container keeps resources that are not
used anywhere and they are in inactive state but ready to use.

      +-Pool-----------{0,0}-+
      |                      |
      | Active--+  Idle----+ |
      | |       |  |       | |
      | |       |  |       | |
      | |       |  |       | |
      | +-------+  +-------+ |
      +----------------------+

We will use a diagram above to explain operations with pool in following text. Symbols in right of first
line `-{0,0}-` show the load of containers: `-{N_active,N_idle}-`,

 where:
<ul style="list-style-type:none">
<li>`N_active` - number of active resorces;</li>
<li>`N_idle` - number of idle resources.</li>
</ul>

## Operations

First thing we have to do is create an instance of resource pool.

  {ok, Pid} = resource_pool:new(test_pool, resource_factory, resource_metadata)

`test_pool` is a registered name for the new pool and `resource_factory` is a name of a module 
that implements resource_factory behaviour. Now we can use `test_pool`
or `Pid` as a reference to pool instance. Resource factory module will be responsible for creating, checking and disposing
of resource instances and is discussed in details in [Resource factory](#resource-factory) section below.

The common scenario of using of the resource pool is state with a few concurrently running processes shares the same pool to borrow resources from it.

### _borrow_
To retrieve a resource from pool process has to call function `borrow`.

```
  Resource = resource_pool:borrow(test_pool)
```

If `Idle` list is empty the pool creates new resource and grants it to calling process.

      +-Pool-----------{1,0}-+          +-Pool-----------{2,0}-+
      |                      |          |                      |
      | Active--+  Idle----+ |          | Active--+  Idle----+ |
      | |       |  |       | |          | |       |  |       | |
      | |       |  |       | |    =>    | | <R.2> |  |       | |
      | | <R.1> |  |       | |          | | <R.1> |  |       | |
      | +-------+  +-------+ |          | +-------+  +-------+ |
      +----------------------+          +----------------------+

If the pool has idle resource within `Idle` list an idle resource just transfers to `Active` list and
it is granted to caling process.

      +-Pool-----------{1,2}-+          +-Pool-----------{2,1}-+
      |                      |          |                      |
      | Active--+  Idle----+ |          | Active--+  Idle----+ |
      | |       |  |       | |          | |       |  |       | |
      | |       |  | <R.2> | |    =>    | | <R.2> |  |       | |
      | | <R.1> |  | <R.3> | |          | | <R.1> |  | <R.3> | |
      | +-------+  +-------+ |          | +-------+  +-------+ |
      +----------------------+          +----------------------+

### _return_
Process has to return a resource to the pool after the process completes using a resource.
In other words the resource is moved from `Active` list to `Idle` list. Now other concurrent 
processes can borrow freed resource from the pool.

      +-Pool-----------{2,1}-+          +-Pool-----------{1,2}-+
      |                      |          |                      |
      | Active--+  Idle----+ |          | Active--+  Idle----+ |
      | |       |  |       | |          | |       |  |       | |
      | | <R.2> |  |       | |    =>    | |       |  | <R.2> | |
      | | <R.1> |  | <R.3> | |          | | <R.1> |  | <R.3> | |
      | +-------+  +-------+ |          | +-------+  +-------+ |
      +----------------------+          +----------------------+

### _add_
Sometimes we need just add new resource to pool. Function `add` creates new resource and
puts it into `Idle` list. 

      +-Pool-----------{2,1}-+          +-Pool-----------{2,2}-+
      |                      |          |                      |
      | Active--+  Idle----+ |          | Active--+  Idle----+ |
      | |       |  |       | |          | |       |  |       | |
      | | <R.2> |  |       | |    =>    | | <R.2> |  | <R.4> | |
      | | <R.1> |  | <R.3> | |          | | <R.1> |  | <R.3> | |
      | +-------+  +-------+ |          | +-------+  +-------+ |
      +----------------------+          +----------------------+

### _invalidate_
If resource failed then a process has to let know about it to the pool. `invalidate` function marks failed resource
as unusable and pool will be destroy it shortly.

      +-Pool-----------{2,1}-+          +-Pool-----------{1,1}-+
      |                      |          |                      |
      | Active--+  Idle----+ |          | Active--+  Idle----+ |
      | |       |  |       | |          | |       |  |       | |
      | | <R.2> |  |       | |    =>    | |       |  |       | |
      | | <R.1> |  | <R.3> | |          | | <R.1> |  | <R.3> | |
      | +-------+  +-------+ |          | +-------+  +-------+ |
      +----------------------+          +----------------------+

### _typical use case_
Suppose that `resource` module implements some operations under resource.

```
  case resource_pool:borrow(test_pool) of
    {error, E} -> io:format("Error while borrow from pool, reason: ~p", [E]);
    Resource ->
      try
        resource:operation(Resource),
        resource_pool:return(test_pool, Resource)
      catch
        _:_ -> resource_pool:invalidate(test_pool, Resource)
      end,
  end
```

If everything is going well we see flow like this: borrow --> use --> return. When something wrong is happened during 
resource use then we have other flow: borrow --> use --> invalidate.
 
## Size limits
We can setup some features and parameters for a resource pool during instantiation by using `option` parameter
of `new` operation (see [new](#_new_)):

```
  {ok, Pid} = resource_pool:new(test_pool, resource_factory, resource_metadata, options)
```

`Options` list contains a few values those define scales, limitation and behavior of a pool. Some of those are
responsible for size of `Active` and `Idle` containers:

 max_active,
 max_idle,
 min_idle

```
             +-Pool-----------{0,0}-+
             |                      |
             | Active--+  Idle----+ |
             | |       |  |_______|_|__ max_idle
 max_active__|_|_______|  |       | |
             | |       |  |       | |
             | |       |  |_______|_|__ min_idle
             | |       |  |       | |
             | +-------+  +-------+ |
             +----------------------+
```

### max_active 
Maximum size of `Active` list is 8 by default. If it reaches the limit following `borrow` operation will be blocked or 
failed (see [Borrow with exhausted pool](#_borrow-with-exhausted-pool_) for details). The value -1 (or any negative) means no limitation on `Active` list size.
Example of use: 

```
  {ok, Pid} = resource_pool:new(test_pool, resource_factory, [], [{max_active, 20}])
```

### max_idle
Maximum size of `Idle` list equals max_active by default. If it reaches the limit then following `return` operation 
will be finished with destroying of the returned resource. The value -1 (or any negative) means no limitation on `Idle` list maximum size.
Example of use: 

```
  {ok, Pid} = resource_pool:new(test_pool, resource_factory, [], [{max_active, 20}, {max_idle, 10}])
```

### min_idle
Minimum size of `Idle` list is 0 by default. If it reaches the limit then following `borrow` operation will
successfully supplies a resource to invoker and then pool will additionally create new resource in `Idle` container to provide 
min_idle condition. The value -1 (or any negative) means no limitation on Idle list minimum size.
Example of use: 

```
  {ok, Pid} = resource_pool:new(test_pool, resource_factory, [], [{max_active, 20}, {max_idle, 10}, {min_idle, 3}])
```

## Behaviour options
### _Borrow with exhausted pool_
When we set max_active greater then 0 and size of Active list reaches this value then the pool is exhausted and pool's behaivior depends on when_exhausted_action option value:
<dl>
<dt> {when_exhausted_action, fail}</dt><dd>`borrow` function on exhausted pool returns {error, pool_exhausted}.</dd>
<dt> {when_exhausted_action, block}</dt><dd>`borrow` function on exhausted pool is blocked until a new or idle object is available.
 Waiting time period is limited by value of other option max_wait (see [Timing](#_timing_)).</dd>
<dt> {when_exhausted_action, grow}</dt><dd>`borrow` function on exhausted pool returns new resource and size of `Active` list grows. In this case `max_Idle` option is just ignored.</dd>
</dl>
Default value is `block`. 
Example of use: 

```
  {ok, Pid} = resource_pool:new(test_pool, resource_factory, [], [{max_active, 20}, {when_exhausted_action, fail}])
```

### _Resource checking_
Resource pool can check status of managed resources. Options `test_on_borrow` and `test_on_return`
control how pool tests resources: before providing resource to invoker `{test_on_borrow, true}` and after a resource was returned
to pool `{test_on_return, true}`. If pool finds that the resource is not alive during test then the resource will be destroyed.

### _Resource order in idle container_
Option `fifo` (first-input-first-output) controls order of extracting a resources from `Idle` list. Diagrams below illustrate this. Suppose we
fill out `Idle` list in order: <R.1> was first, <R.2> is next, then <R.3>. Resource <R.4> is active in given moment. If
`{fifo, true}` is set the `borrow` operation leads to situation below: resource <R.1> was came first and
it becomes active now (first out).

      +-Pool-----------{1,2}-+          +-Pool-----------{2,1}-+
      |                      |          |                      |
      | Active--+  Idle----+ |          | Active--+  Idle----+ |
      | |       |  | <R.3> | |          | |       |  |       | |
      | |       |  | <R.2> | |    =>    | | <R.1> |  | <R.3> | |
      | | <R.4> |  | <R.1> | |          | | <R.4> |  | <R.2> | |
      | +-------+  +-------+ |          | +-------+  +-------+ |
      +----------------------+          +----------------------+

If `{fifo, false}` is set it means that order will be last-input-first-output. `borrow` operation makes active resource
<R.3> (last input).

      +-Pool-----------{1,2}-+          +-Pool-----------{2,1}-+
      |                      |          |                      |
      | Active--+  Idle----+ |          | Active--+  Idle----+ |
      | |       |  | <R.3> | |          | |       |  |       | |
      | |       |  | <R.2> | |    =>    | | <R.3> |  | <R.2> | |
      | | <R.4> |  | <R.1> | |          | | <R.4> |  | <R.1> | |
      | +-------+  +-------+ |          | +-------+  +-------+ |
      +----------------------+          +----------------------+

Default value for `fifo` is `false`.

### _Timing_
`max_wait` option defines the maximum amount of time to wait when the `borrow` function is invoked,
the pool is exhausted and `when_exhausted_action` equals `block`.

`max_idle_time` option defines non terminated period of time an resource instance may sit idle in the pool, 
with the extra condition that at least `min_idle` amount of object remain in the pool. No resources 
will be evicted from the pool due to maximum idle time limit if `max_idle_time` equals `infinity`.

## Maintenance of pool instance
### _new_
Lets look more closely at resource pool instantiation. `pool_name` is atom and multiple processes can use the
registered name to access the resource pool. `resource_factory` is module name that is responsible for creating and maintenance
of a resources. `resource_metadata` is an object that contains information for instantiation of an resource. The object is passed
as parameter to each function of `resource_factory` to help maintain an resources. 

```
  {ok, Pid} = resource_pool:new(pool_name, resource_factory, resource_metadata)
```

### _clear_
The function sweep up (destroy) all resources from pool.

```
  ok = resource_pool:clear(pool_name)
```

### _close_
The function terminates pool process and destroys all resources from pool.

```
  ok = resource_pool:close(pool_name)
```

### _get_num_active, get_num_idle, get_number_
The functions return number of resources in `Active`, `Idle` containers and total number of resources.

## Resource factory
Before we do not go in details of an resources managed by pool. We was thinking about its as abstract resource without any
features and properties. It is not true in reality. Real resources (as connections, sockets, channels and so on) are living in pool
are composed objects with number of properties and they have an life cycle: we have to create them, test, use and dispose them.
Resource factory separate pool functionality from managed resources functionality and allows to easy customize pool for
different types of resources.

`resource_factory` module defines `behavior` of generic resource factory. We have to implement this 
`behavior` while designing of resource factory module for given resource. The module has to consist following functions:
<dl>
<dt> `create(Resource_metadata::term())` </dt><dd> The function creates new instance of the resource. In Erlang word this is a new
 process in most cases. `Resource_metadata` is a data structure that describes an resource. `Resource_metadata` came
 to the pool from `new` operation and it has to be enough to create and manage the resource. Structure and contain of
 the `Resource_metadata` is custom and it is used only by `resource_factory` but is kept as a pool state.</dd>
<dt> `destroy(Resource_metadata::term(), Resource::pid())` </dt><dd> The function destroys the resource represented by `Resource` as a `pid`.</dd>
<dt> `validate(Resource_metadata::term(), Resource::pid())` </dt><dd> The function check an `Resource` and returns true if the resource is valid.</dd>
<dt> `activate(Resource_metadata::term(), Resource::pid())` </dt><dd> The function is callback that is fired when pool are moving `Resource` from
 passive state to active (from idle list to active list).</dd>
<dt> `passivate(Resource_metadata::term(), Resource::pid())` </dt><dd> The function is callback that is fired when pool are moving `Resource` from
 active state to passive (from active list to idle list).</dd>
</dl>

## Examples
### _Connection pool for MySQL driver_
[Erlang MySQL client](http://sourceforge.net/projects/erlmysql)
### _Channel pool for Rabbit MQ connection_
[AMQP channel pool example](http://sourceforge.net/projects/erlpool/files/1.0.x/erl.resource.pool.example.zip/download)