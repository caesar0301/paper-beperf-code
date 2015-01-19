BePerf
------

A MapReduce project written in Java to serialize (in avro),
classify users and detect sessions etc.

`pom.xml`
---------

Two sets of pom configuration is included for different
running environments:

* `pom-apache.xml`: pom file for Apache Hadoop v1.
* `pom-cdh.xml`: pom file for Cloudera Hadoop v2.

To import the project into Eclipse or IntelliJ IDE, first
create a `pom.xml` soft link manually after cloning, e.g.:

    ln -s pom-cloudera.xml pom.xml

Compile
-------

    $ mvn package

Running MR Jobs
---------------

    $java -cp target/beperf*.jar \
    cn.edu.sjtu.omnilab.beperf.<APPNAME> \
    [parameters]

Several APPNAMEs are shipped with the distribution for data preprocessing:

* `DataSerializeMR`: Serialize the raw traffic logs into avro format.

* `UserClassifyMR`: Classify users (in avro format) as mobile or non-mobile
and store them into separated folders.

* `AnomalyDetectMR`: Obtain statistics of abnormal user activities from
traffic logs (in avro format).

Copyright
---------

Xiaming Chen - chenxm35@gmail.com
License: MIT
