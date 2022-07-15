## Grammar Rules

```program -> {stmt_list} $$
stmt_list -> stmt stmt_list
stmt_list -> epsilon
stmt -> id = expr ; | if (expr) stmt | read id ; | write expr ;
expr -> id etail | num etail
etail -> + expr | - expr | epsilon
id -> [a-zA-Z]+
num -> numsign digit digit*
numsign -> + | - | epsilon```
