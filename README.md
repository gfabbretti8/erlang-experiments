# Exploring Distributed Erlang Behaviors With Docker

This repository contains several different implementations of Erlang systems running in a Docker environment.

The objective is to understand how the language behaves in ("not-so much") corner cases which are not covered by the official documentation.

**Caveat** : The code here has been tested on a virtual machine running Ubuntu 16.04 and it has not been tested extensively on different environments, hence some adjusting may be required to run the experiments on different settings. 

To simulate a distributed environment Erlang nodes will be running on dockers'
container connected through a network. By using docker facilities we will
simulate failure and changes in the network structure. 

### Pinging a reincarnation of a previous known location

**Description**. In this scenario $l_2$ successfully contacts $l_1$, thus
establishing a connection. Then, $l_1$ fails and recovers. Finally, $l_2$,
before detecting that the incarnation of $l_1$ to which it was connected has
failed, tests again its accessibility. 

The following commands set up the configuration and attach a remote shell to
$l_2$.

```
  gfabbret@ubuntu:~/$ docker-compose up -d
  gfabbret@ubuntu:~/$ docker exec -it l2.com erl -name test@l2.com
                      -setcookie cookie -remsh app@l2.com -hidden
```

Then, to establish a connection between the two locations in the Erlang console
we can ping the remote location.

```
  (app@l2.com)2> net_adm:ping('app@l1.com').
  pong
```

To induce the fault and make it seem like a genuine interruption of services,
without third parties being notified, we detach the container from all the
networks, restart it, and reconnect it to the networks it was connected to. The
disconnection is required as otherwise the other containers would be notified of
the restart. Detaching and re-attaching the container to the network takes few
milliseconds, hence not enough for the Erlang system to detect it. To do so we
get the container and network ids through the docker commands and we feed them
to the restart script.

```
gfabbret@ubuntu:~/scenario$ docker container ls
CONTAINER ID  IMAGE          COMMAND     CREATED          STATUS         NAMES
ba5915332d87  erlang:25.0.3  "erl ..."   49 seconds ago   Up 49 seconds  l2.com
76c89a08761c  erlang:25.0.3  "erl ..."   49 seconds ago   Up 49 seconds  l1.com
gfabbret@ubuntu:~/scenario-4$ docker network ls
NETWORK ID     NAME                              DRIVER    SCOPE
fec145052743   bridge                            bridge    local
1b349490c51c   host                              host      local
373b2aa1c5e9   none                              null      local
cfc05f57eeee   scenario_net1                   bridge    local
gfabbret@ubuntu:~/scenario$ ./restart.sh cfc05f57eeee 76c89a08761c
ba5915332d87
done
```

Finally we attempt to ping the remote location after the restarting.

```
  ...
  (app@l2.com)3> spawn('app@l1.com', fun() -> self() end).
  <0.107.0>
  (app@l2.com)4> =WARNING REPORT==== 2-May-2023::15:16:02.174308 ===
  ** Can not start erlang:apply,[#Fun<erl_eval.43.3316493>,[]] on 'app@l1.com' **
```

The test for spawn failed even if there is a live running instance of
$l_1$. The reason why is that $l_2$ attempted to spawn on the instance of
$l_2$ that it knew already (in terms of our calculus, location $l_2$ at incarnation $\lambda$), which was stored in its view, and not the one
currently alive (location $l_2$ at incarnation $\lambda+1$). If we had given $l_2$ enough time it would have detected that
the incarnation of $l_2$ it knew was dead since it \emph{would not feel its
  heartbeat} and in that case the spawn would have succeeded since $l_1$ would
have initiated a new connection.



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

Now, let us discuss the more interesting \textbf{servDFV}, where the misbehavior
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
