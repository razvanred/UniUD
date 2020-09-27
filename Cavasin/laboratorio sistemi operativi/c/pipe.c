#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>

#define READ 0
#define WRITE 1

/*
 * Questo programma esegue l'equivalente del comando shell:
 *   ls -l | grep <pattern>
 * dove <pattern> viene specificato dalla riga di comando
 */
int main(int argc, char **argv) {
  int pipes[2];
  if(pipe(pipes) == -1) {
    perror("pipe()");
  }

  pid_t ls= fork();
  switch(ls) {
    case -1:
      perror("fork()");
    case 0:  // figlio
      close(pipes[READ]);
      dup2(pipes[WRITE], STDOUT_FILENO);  // redirigo stdout
      execlp("ls", "ls", "-l", NULL);
      perror("ls");  // se arrivo qui, execlp() ha fallito
      return 2;
  }  // il padre prosegue

  pid_t grep= fork();
  switch(grep) {
    case -1:
      perror("fork()");
    case 0:  // figlio
      close(pipes[WRITE]);
      dup2(pipes[READ], STDIN_FILENO);  // redirigo stdin
      execlp("grep", "grep", argv[1], NULL);
      perror("grep");  // se arrivo qui, execlp() ha fallito
  }                    // il padre prosegue

  close(pipes[0]);
  close(pipes[1]);
  // aspetto entrambi i figli
  waitpid(ls, NULL, 0);
  waitpid(grep, NULL, 0);
  return 0;
}