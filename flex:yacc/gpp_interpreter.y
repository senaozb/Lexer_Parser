%{
      #include <stdio.h>
      #include <string.h>

      int flag_exit = 0;
      int error_flag = 0;
      int fcall_flag = 0;
      int yylex();
      void yyerror(char* err) {printf("SYNTAX_ERROR Expression not recognized\n");
                            flag_exit = 1;}
      extern FILE *yyin;
      int number[2];
      int result[2];

      //---- ID Table -------//
      char idname[1000][50];
      int idvalue[1000][2];
      int counter_id = 0;
      //---------------------//

      //--- Function Table ---//
      char func_name[1000][50];
      int func_param[1000];
      char func_param_name[1000][3][50];
      char* func;
      int counter_func = 0;
      //----------------------//


      int gcd_calculater(int n, int m);
      void simplify();
      void plus(int up1, int down1, int up2, int down2);
      void minus(int up1, int down1, int up2, int down2);
      void multip(int up1, int down1, int up2, int down2);
      void div(int up1, int down1, int up2, int down2);
      void extract(char p[50], int len);
      void assign(char name[50], int up, int down);
      void check_id(char name[50], int up, int down, int op);
      void find_id(char name[50]);
      void func_eval(char name[50], int param, char p1[50], char p2[50], char p3[50]);
      void func_resolve(char name[50], int param, char param1[2][50], char param2[2][50], char param3[2][50]);
      void func_replace(char name[50], int index, char param1[2][50], char param2[2][50], char param3[2][50]);

%}

%start START
%token DEFV DEFF KW_WHILE KW_IF KW_EXIT KW_TRUE KW_FALSE
%token OP_PLUS OP_MINUS OP_DIV OP_MULT OP CP OP_SET OP_COMMA OP_AND OP_OR OP_NOT OP_EQ OP_GT
%token COMMENT 
%token NL


%union {
      struct  
      {
            int up;
            int down;
            char word[50];
            int length;
            int boolean_val;
            char string_exp[10000];
            int func_flag;
      }content;

}

%type <content> EXPLIST
%type <content> EXPLISTIN
%type <content> EXP
%type <content> ASG
%type <content> FUNCTION
%type <content> EXPB

%token <content> VALUEF
%token <content> ID



 
%%
START:      INPUT          { return 1;}
      |     OP KW_EXIT CP  { flag_exit = 1; return 0;}
      ;
       
INPUT:    
            FUNCTION      { if(error_flag == 0) 
                            { printf("\nSYNTAX OK\n"); 
                              printf("\nResult = %d / %d\n", $1.up, $1.down); }}
      |     EXP           { if(error_flag == 0 && fcall_flag == 0)
                            { printf("\nSYNTAX OK\n"); 
                              printf("\nResult = %d / %d\n", $1.up, $1.down); }
                            else fcall_flag = 0;}
      |     EXPLIST       { if(error_flag == 0)
                            { printf("\nSYNTAX OK\n"); 
                              printf("\nResult = %d / %d\n", $1.up, $1.down); }}
      ;

FUNCTION:
            OP DEFF ID OP CP EXPLIST CP             { func_eval($3.word, 0, "", "", "");
                                                      $$.up = 0;
                                                      $$.down = 1;}
      |     OP DEFF ID OP ID CP EXPLIST CP          { func_eval($3.word, 1, $5.word, "", "");
                                                      $$.up = 0;
                                                      $$.down = 1;}
      |     OP DEFF ID OP ID ID CP EXPLIST CP       { func_eval($3.word, 2, $5.word, $6.word, "");
                                                      $$.up = 0;
                                                      $$.down = 1;}
      |     OP DEFF ID OP ID ID ID CP EXPLIST CP    { func_eval($3.word, 3, $5.word, $6.word, $7.word);
                                                      $$.up = 0;
                                                      $$.down = 1;}
      ;


EXPLIST:
            OP EXPLISTIN CP       {$$.up = $2.up;
                                   $$.down = $2.down;}
      ;

EXPLISTIN:  
            EXPLISTIN EXP             {$$.up = $2.up;
                                       $$.down = $2.down;}
      |     EXP EXP                   {$$.up = $2.up;
                                       $$.down = $2.down;}
      ;

EXP:  
            OP OP_PLUS EXP EXP CP               { plus($3.up, $3.down, $4.up, $4.down); 
                                                  $$.up = result[0];
                                                  $$.down = result[1];}
      |     OP OP_MINUS EXP EXP CP              { minus($3.up, $3.down, $4.up, $4.down);
                                                  $$.up = result[0];
                                                  $$.down = result[1];}
      |     OP OP_MULT EXP EXP CP               { multip($3.up, $3.down, $4.up, $4.down); 
                                                  $$.up = result[0];
                                                  $$.down = result[1];}
      |     OP OP_DIV EXP EXP CP                { div($3.up, $3.down, $4.up, $4.down); 
                                                  $$.up = result[0];
                                                  $$.down = result[1];}
      |     ID                                  { if($1.func_flag == 0) find_id($1.word);
                                                  $$.up = number[0];
                                                  $$.down = number[1];}
      |     VALUEF                              { extract($1.word, $1.length);
                                                  $$.up = number[0];
                                                  $$.down = number[1];}
      |     FCALL                               { fcall_flag = 1;}
      |     ASG                                 { $$.up = $1.up;
                                                  $$.down = $1.down;}
      |     OP KW_IF EXPB EXPLIST EXPLIST CP    { if($3.boolean_val == 1) {$$.up = $4.up; $$.down = $4.down;}
                                                       else   {$$.up = $5.up; $$.down = $5.down;}}
      |     OP KW_WHILE EXPB EXPLIST CP         { if($3.boolean_val == 1) {$$.up = $4.up; $$.down = $4.down;}}
      |     OP DEFV ID EXP CP                   { check_id($3.word, $4.up, $4.down, 1);
                                                  $$.up = number[0];
                                                  $$.down = number[1];}
      ;


EXPB:
            OP OP_EQ EXP EXP CP                 { minus($3.up, $3.down, $4.up, $4.down);
                                                  if(result[0] == 0) $$.boolean_val = 1;
                                                            else    $$.boolean_val = 0;}
      |     OP OP_GT EXP EXP CP                 { minus($3.up, $3.down, $4.up, $4.down);
                                                  if(result[0] < 0 || result[1] < 0) $$.boolean_val = 0;
                                                                             else    $$.boolean_val = 1;}
      |     KW_TRUE                             { $$.boolean_val = 1;}
      |     KW_FALSE                            { $$.boolean_val = 0;}
      |     OP OP_AND EXPB EXPB CP              { $$.boolean_val = ($3.boolean_val && $4.boolean_val);}
      |     OP OP_OR EXPB EXPB CP               { $$.boolean_val = ($3.boolean_val || $4.boolean_val);}
      |     OP OP_NOT EXPB CP                   { if($3.boolean_val == 0) $$.boolean_val = 1;
                                                  else $$.boolean_val = 0 ;}
      ;     


ASG:
            OP OP_SET ID EXP CP                 { check_id($3.word, $4.up, $4.down, 0);
                                                  $$.up = number[0];
                                                  $$.down = number[1];}
      ;

FCALL:
            OP ID CP                            {char param1[2][50]; 
                                                      strcpy(param1[0], "");
                                                      strcpy(param1[1], "");
                                                 char param2[2][50]; 
                                                      strcpy(param2[0], "");
                                                      strcpy(param2[1], "");
                                                 char param3[2][50]; 
                                                      strcpy(param3[0], "");
                                                      strcpy(param3[1], "");
                                                 func_resolve($2.word, 0, param1, param2, param3); }
      |     OP ID EXP CP                        {char str[50], str2[50];
                                                 sprintf(str, "%d", $3.up);
                                                 sprintf(str2, "%d", $3.down);
                                                 char param1[2][50]; 
                                                      strcpy(param1[0], str);
                                                      strcpy(param1[1], str2);
                                                 char param2[2][50]; 
                                                      strcpy(param2[0], "");
                                                      strcpy(param2[1], "");
                                                 char param3[2][50]; 
                                                      strcpy(param3[0], "");
                                                      strcpy(param3[1], "");
                                                 func_resolve($2.word, 1, param1, param2, param3); }
      |     OP ID EXP EXP CP                    {char str[50], str2[50], str3[50], str4[50];
                                                 sprintf(str, "%d", $3.up);
                                                 sprintf(str2, "%d", $3.down);
                                                 sprintf(str3, "%d", $4.up);
                                                 sprintf(str4, "%d", $4.down);
                                                 char param1[2][50]; 
                                                      strcpy(param1[0], str);
                                                      strcpy(param1[1], str2);
                                                 char param2[2][50]; 
                                                      strcpy(param2[0], str3);
                                                      strcpy(param2[1], str4);
                                                 char param3[2][50]; 
                                                      strcpy(param3[0], "");
                                                      strcpy(param3[1], "");
                                                 func_resolve($2.word, 2, param1, param2, param3); }
      |     OP ID EXP EXP EXP CP                {char str[50], str2[50], str3[50], str4[50], str5[50], str6[50];
                                                 sprintf(str, "%d", $3.up);
                                                 sprintf(str2, "%d", $3.down);
                                                 sprintf(str3, "%d", $4.up);
                                                 sprintf(str4, "%d", $4.down);
                                                 sprintf(str5, "%d", $5.up);
                                                 sprintf(str6, "%d", $5.down);
                                                 char param1[2][50]; 
                                                      strcpy(param1[0], str);
                                                      strcpy(param1[1], str2);
                                                 char param2[2][50]; 
                                                      strcpy(param2[0], str3);
                                                      strcpy(param2[1], str4);
                                                 char param3[2][50]; 
                                                      strcpy(param3[0], str5);
                                                      strcpy(param3[1], str6);
                                                 func_resolve($2.word, 3, param1, param2, param3); }
      ;


%%

//------------FUNCTION PART STARTS-------------
      // Greatest common divisor to simplify the fraction
      int gcd_calculater(int n, int m)
      {
            int gcd, remainder;

            while (n != 0)
            {
                  remainder = m % n;
                  m = n;
                  n = remainder;
            }

            gcd = m;

            return gcd;
      }

      void simplify()
      {
            int n1 = result[0] / gcd_calculater(result[0], result[1]);
            int n2 = result[1] / gcd_calculater(result[0], result[1]);
            result[0] = n1;
            result[1] = n2;
      }

      //Arithmetic Operations
      void plus(int up1, int down1, int up2, int down2)
      {
            result[0] = up1*down2 + up2*down1;
            result[1] = down1*down2; 
            simplify();
      }

      void minus(int up1, int down1, int up2, int down2)
      {
            result[0] = up1*down2 - up2*down1;
            result[1] = down1*down2; 
            simplify();
      }

      void multip(int up1, int down1, int up2, int down2)
      {
           result[0] = up1*up2;
           result[1] = down1*down2;
           simplify();
      }

      void div(int up1, int down1, int up2, int down2)
      {
           result[0] = up1*down2;
           result[1] = down1*up2;
           simplify();
      }

      //Extract the value of the fraction from the string
      void extract(char p[50], int len)
      {
            int up = 0, down = 0, i, f = 0;

            for(i = 0; i < len; i++)
            {
                  if(f == 0 && p[i] != 'f')
                  {
                        up *= 10;
                        up += (int) p[i] - '0';
                  }
                  else if(p[i] == 'f')
                        f = 1;
                  else
                  {
                        down *= 10;
                        down += (int) p[i] - '0';
                  }

            }

            number[0] = up;
            number[1] = down;
      }

      //Assignment operation
      void assign(char name[50], int up, int down)
      {     
            if(counter_id < 1000)
            {
                  strcpy(idname[counter_id], name);
                  idvalue[counter_id][0] = up;
                  idvalue[counter_id][1] = down;
                  counter_id++;
            }
      }

      //Check if the given ID is in the table already
      //If not, assign it by adding to table
      void check_id(char name[50], int up, int down, int op)
      {     
            int i, flag = 0;
            for(i = 0; i < counter_id; i++)
            {
                  if(strcmp(name, idname[i]) == 0)
                  {
                        if(op == 0)
                        {
                              idvalue[i][0] = up;
                              idvalue[i][1] = down;
                        }
                        flag = 1;
                  }
            }

            if(flag == 0)
                  assign(name, up, down);

      }

      //Check if the given ID is in the table already
      //If not, raise an error
      void find_id(char name[50])
      {
            int i, flag = 0;
            for(i = 0; i < counter_id; i++)
            {
                  if(strcmp(name, idname[i]) == 0)
                  {
                        number[0] = idvalue[i][0];
                        number[1] = idvalue[i][1];
                        flag = 1;
                  }
            }

            if(flag == 0)
            {
                  error_flag = 1;
                  yyerror(name);
            }

      }

      //Store the given function if it is not available
      void func_eval(char name[50], int param, char p1[50], char p2[50], char p3[50])
      {
            int i, flag = 0;
            for(i = 0; i < counter_func; i++)
            {
                  if(strcmp(name, func_name[i]) == 0)
                        flag = 1;
            }
            
            if(flag == 0)
            {
                  if(counter_func < 1000)
                  {
                        strcpy(func_name[counter_func], name);
                        func_param[counter_func] = param;
                        strcpy(func_param_name[counter_func][0], p1);
                        strcpy(func_param_name[counter_func][1], p2);
                        strcpy(func_param_name[counter_func][2], p3);
                        counter_func++;

                        FILE *fptr;
                        strcat(name, ".txt");
                        
                        fptr = fopen(name,"w");

                        fprintf(fptr,"%s", yylval.content.string_exp);
                        fclose(fptr);
                  }
            }
      }

      //Extract the expression list part of the function and replace the parameters 
      //with given values in function call and send to parser again for re-evaluation
      void func_replace(char name[50], int index, char param1[2][50], char param2[2][50], char param3[2][50])
      {
            int counterOP = 0;
            int counterCP = 0;
            int i = 0;
            char explist[10000];
            int explist_counter = 0;

            int c_1 = 0, c_2 = 0, c_3 = 0;

            //Store the expression list part and perform replacement
            while((counterOP != counterCP) || (counterOP == 0))
            {
                  if(func[i] == '(')
                        counterOP++;
                  else if(func[i] == ')')
                        counterCP++;      
                  
                  if((counterCP > 1) || ((counterCP == 1 && func[i] != ')')))
                  {
                        if(func_param_name[index][0][c_1] == func[i])
                              c_1++;
                        else if(func_param_name[index][1][c_2] == func[i])
                              c_2++;
                        else if(func_param_name[index][2][c_3] == func[i])
                              c_3++;
                        else
                        {
                              c_1 = 0;
                              c_2 = 0;
                              c_3 = 0;
                              explist[explist_counter] = func[i];
                              explist_counter++;
                        }
                         
                        if(c_1 != 0 && func_param_name[index][0][c_1] == '\0')
                        {
                              int j = 0;
                              while(param1[0][j] != '\0')
                              {
                                    explist[explist_counter] = param1[0][j];
                                    explist_counter++;
                                    j++;
                              }

                              j = 0;
                              explist[explist_counter] = 'f';
                              explist_counter++;

                              while(param1[1][j] != '\0')
                              {
                                    explist[explist_counter] = param1[1][j];
                                    explist_counter++;
                                    j++;
                              }
                        }
                        else if(c_2 != 0 && func_param_name[index][1][c_2] == '\0')
                        {
                              int j = 0;
                              while(param2[0][j] != '\0')
                              {
                                    explist[explist_counter] = param2[0][j];
                                    explist_counter++;
                                    j++;
                              }
                              
                              j = 0;
                              explist[explist_counter] = 'f';
                              explist_counter++;

                              while(param2[1][j] != '\0')
                              {
                                    explist[explist_counter] = param2[1][j];
                                    explist_counter++;
                                    j++;
                              }
                        }
                        else if(c_3 != 0 && func_param_name[index][2][c_3] == '\0')
                        {
                              int j = 0;
                              while(param3[0][j] != '\0')
                              {
                                    explist[explist_counter] = param3[0][j];
                                    explist_counter++;
                                    j++;
                              }
                              
                              j = 0;
                              explist[explist_counter] = 'f';
                              explist_counter++;

                              while(param3[1][j] != '\0')
                              {
                                    explist[explist_counter] = param3[1][j];
                                    explist_counter++;
                                    j++;
                              }
                        }
                  }

                  i++;
            }

            explist_counter--;


            FILE *fptr;

            strcat(name, "Mod.txt");     
            fptr = fopen(name,"w");
            for(int i = 0; i < explist_counter; i++)
            {
                  fprintf(fptr, "%c", explist[i]);
            }
            fclose(fptr);

            fptr = fopen(name,"r");
            yyin = fptr;
            yyparse();
            fclose(fptr);
            yyin= stdin;
      }

      //Fetch the function from its file and send it for execution
      void func_resolve(char name[50], int param, char param1[2][50], char param2[2][50], char param3[2][50])
      {
            int i, flag = 0, index;
            long size;
            for(i = 0; i < counter_func; i++)
            {
                  if(strcmp(name, func_name[i]) == 0 && func_param[i] == param)
                  {     
                        flag = 1;
                        index = i;
                  }
            }

            if(flag == 1)
            {
                  FILE *fptr;
                  char mod[50];
                  strcpy(mod, name);
                  strcat(mod, ".txt");
                        
                  fptr = fopen(mod,"r");
                  if(fptr != NULL)
                  {
                        fseek(fptr, 0, SEEK_END); 
                        size = ftell(fptr);
                        fseek(fptr, 0, SEEK_SET); 
                        func = malloc(size);
                        fread(func, 1, size, fptr);
                  }
                        
                  fclose(fptr);

                  func_replace(name, index, param1, param2, param3);
            }
            else
            {
                  error_flag = 1;
                  yyerror(name);
            }

      }


//------------FUNCTION PART ENDS-------------

      int main(int argc , char** argv)
      {
            if(argc == 1) 
            {
                  yyin = stdin; 
                  while (flag_exit == 0)
                  {
                        printf("\nType anything or type (exit) to terminate:");
                        yyparse();
                        yylval.content.up = 0;
                        yylval.content.down = 0;
                        strcpy(yylval.content.word, "");
                        yylval.content.length = 0;
                        yylval.content.boolean_val = 0;
                        strcpy(yylval.content.string_exp, "");
                        yylval.content.func_flag = 0;
                  }
            }
            else 
            {
                  printf("Invalid argument number\n");
                  return 0;
            }
            
            return 0;
      }

