# Resource Pool (rsrc_pool)
Resource pool project is written in Erlang as a tiny library. The goal of the tool is reduce the overhead of creating new resources by reusing of the same resources among multiple processes. Achieving result is better performance and throughput. The resource pool was inspired by Java Apache's commons pool and adopts API and main principals from this project. Database connection is most popular example for pooling resource.

## Introduction
Resource pool project was inspired by Apache Commons Pool library and API was borrowed from there. But internal 
implementation is completely different and is using Erlang OTP design principles and Erlang concurrent model. Resource Pool
is Erlang application library.   

## Structure
<ul>
  <li><code>resource_pool_srv.erl</code> is a main module of Resource Pool. It is generic server and implements
almost all Pool functionality.</li>
  <li><code>resource_pool.erl</code> is a facade for gen_server and exposes all API functions.</li>
  <li><code>resource_factory.erl</code> defines <i>resource_factory</i> behaviour and acts as a template and 
simple implementation resource factory for testing purpose.</li>
</ul>

## Getting started
<ol>
  <li>Create instance of Resource Pool<br/> 
    <pre>{ok, Pid} = resource_pool:new(test_pool, resource_factory, [])</pre>
    where: 
    <ul style="list-style-type:none;">
      <li><code>test_pool</code> - registered name of the new pool;</li>
      <li><code>resource_factory</code> - name of a module that implements resource_factory behaviour.</li>
    </ul>
    New resource pool is usually shared between few processes.  
  </li>
  <li>Borrow resource from pool<br/>
    <pre>Resource = resource_pool:borrow(test_pool)</pre>
    The process can use the borrowed resource and has to return to pool after finish. 
  </li>
  <li>Return resource to pool<br/>
      <pre>ok = resource_pool:return(test_pool, Resource)</pre>
      The process cannot use the <code>Resource</code> anymore.
  </li>
  <li>Pool can be created with options:<br/>
    <pre>Options = [{max_active, 10}, {when_exhausted_action, fail}]</pre>
    <pre>{ok, Pid} = resource_pool:new(test_pool, resource_factory, Options)</pre>
    See resource_pool for more details about options. 
  </li>
</ol>

See [Resource Pool](https://erlangcentral.org/wiki/index.php?title=Resource_Pool) article for details and [http://erlpool.sourceforge.net/](http://erlpool.sourceforge.net/).
