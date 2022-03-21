setlocal path+=src/main/java

let g:ale_java_javac_sourcepath = '.:src:test:src/main/java:src/test/java'
let g:ale_java_javac_classpath = getcwd() . '/junit.jar:' . getcwd() . '/lib/junit.jar'
