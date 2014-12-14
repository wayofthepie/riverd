FROM ubuntu:14.10

RUN apt-get upgrade -y && apt-get update -y 

RUN apt-get install -y openjdk-8-jdk maven ssh supervisor

ADD sshd.conf /etc/supervisor/conf.d/

ADD supervisord.conf /etc/supervisor/conf.d/ 

RUN mkdir /var/run/sshd

EXPOSE 22

CMD ["/usr/bin/supervisord"]
