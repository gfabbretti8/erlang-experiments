# Exploring Distributed Erlang Behaviors With Docker

This repository contains several different implementations of Erlang systems running in a Docker environment.

The objective is to understand how the language behaves in ("not-so much") corner cases which are not covered by the official documentation.

**Caveat** : The code here has been tested on a virtual machine running Ubuntu 16.04 and it has not been tested extensively on different environments, hence some adjusting may be required to run the experiments on different settings.

To simulate a distributed environment Erlang nodes will be running on dockers'
container connected through a network. By using docker facilities we will
simulate failure and changes in the network structure.

### Wrong spawn

**Description**. In this scenario location $n$ successfully contacts location $m$, thus
establishing a connection. Then, location $m$ fails and recovers. Finally, location $n$,
before detecting that the incarnation of location $m$ to which it was connected has
failed, attempts to spawns again a process on it.

The following commands set up the configuration and attach a remote shell to
location $n$.

```
  gfabbret@ubuntu:~/$ docker-compose up -d
  gfabbret@ubuntu:~/$ gfabbret@ubuntu:~/$ docker exec -it loc_n.com erl -name test@loc_n.com
                      -setcookie cookie -remsh app@loc_n.com -hidden
```

Similarly, we attach a remote console to location $m$.

```
  gfabbret@ubuntu:~/$ gfabbret@ubuntu:~/$ docker exec -it loc_m.com erl -name test@loc_m.com
                      -setcookie cookie -remsh app@loc_m.com -hidden
```

Then, to establish a connection between the two locations in the Erlang console
we can spawn a process on the remote location.

```
  (app@loc_m.com)2> erlang:spawn(’app@loc_n.com’, erlang, self, []).
```

To induce the fault and make it seem like a genuine interruption of services,
without third parties being notified, we detach the container from all the
networks, restart it, and reconnect it to the networks it was connected to. The
disconnection is required as otherwise the other containers would be notified of
the restart. Detaching and re-attaching the container to the network takes few
milliseconds, hence not enough for the Erlang system to detect it. To do so we
leverage the script restart.

```
gfabbret@ubuntu:~/$ ./restart.sh wrong-spawn_net1 loc_m.com
```

Finally we cab attempt to spawn another process on location $m$ from location $n$ before this last one reacts to the absence of location $m$'s heartbeat.

```
  ...
  (app@loc_n.com)1> erlang:spawn(’app@loc_m.com’, erlang, self, []).
  <0.108.0>
  (app@loc_n.com)2> =WARNING REPORT==== 12-Sep-2024::13:22:40.233966 ===
  ** Can not start erlang:self,[] on ’app@loc_m.com’ **
```

The test for spawn failed even if there is a live running instance of location
$m$. The reason why is that location $n$ attempted to spawn on the instance of
location $m$ that it knew already, which was stored in its view, and not the one
currently alive.

### Running Example in Erlang

We begin by **servD**.

The procedure to set up the three nodes is as above.

Then, we can connect to the interface.

```
  gfabbret@ubuntu:~/$ docker exec -it interface.com erl -name test@interface.com
                      -setcookie cookie -remsh app@interface.com -hidden
```

Then, we can invoke the interface process which will send the request and
observe the outcome.

```
  (app@interface.com)1> servD:interface().
  Initiating request
  Response received
```

As expected, everything went well.

Now, let us discuss the more interesting **servDFV**, where the misbehavior
is due to a spawn that fails because of a wrong view.

The procedure to set up the system is as before, nonetheless this time two
remote consoles are required: one on the interface to start the process and one
on the controller to restart the router. The commands to attach the remote consoles
are as above, but for replacing the name of the container.

Now, we can initiate the request from the interface.

```
  (app@interface.com)1> servDFV:interface().
  Initiating request
```

The process gets stuck because the (bugged) router drops the message. Now, we
can simulate a failure of the router container by means of

```
  gfabbret@ubuntu:~/$ ./restart servdf_net1 router.com
```

Then, right after, before the interface node detects the absence of the router
because of the lack of its \emph{heartbeat} we can restore the service from the
controller console.

```
  (app@controller.com)1> servDFV:controller().
  ok
```

and observe the following behavior on the interface

```
=WARNING REPORT==== 3-May-2023::09:06:33.441402 ===
** Can not start erlang:apply,[#Fun<servDF.1.93666681>,[]] on 'app@router.com'
**
```

where the spawn has failed due to wrong view.

Finally, let us discuss **servDFR**.

To set up the scenario and attach consoles to the interface and the controller
we proceed as above.

Then we start the request
```
  (app@interface.com)1> servDFR:interface().
  Initiating request
```

The process gets stuck like it did in \textbf{servDFV}.
As in \textbf{servDFV} we restart the router and we create a new router through
the controller process. In this implementation though is the router to send a $retry$
message to the interface, hence its view is correctly updated and the following
expected behavior is shown.

```
  Retrying request
  Response received
  ok
```
