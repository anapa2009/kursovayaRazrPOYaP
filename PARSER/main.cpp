
#include <cstdlib>
#include <cstddef>
#include <cstdio>
#include <cstring>
#include <string>
using namespace std;

#include<iostream>
#include <fstream>

#include<vector>


#define NUM_OF_KWORDS 3
#define len_max 32
#define dim_Matr 10

const string keywords[NUM_OF_KWORDS] = {"if", "then", "else"};

char symv[] = {';', '<', '>', ':', '=', '\"', '/'};  //, ':='};

vector <string> mysymv = {";", "if", "then", "else", "a", ":=", "<", ">", "=", "s_first", "s_last"};

const char *filename = "mytest.txt";

enum states {H, ID, LTR, ASGN, DLM, COMMENT, ERR};
enum tok_names {KWORD, IDENT, LITERAL, OPER, DELIM};
const string lex_names[] = {"KWORD", "IDENT", "LITERAL", "OPER", "DELIM"};

vector <pair <int, string> > lexVect, lexIdent;
vector <pair <string, int> > vectSymv; 

// { {";",0},{"if",1},{"then",2},{"else",3},{"a",4},
//   {":=",5},{"<",6},{">",7},{"=",8},{"s_first",9},{"s_last",10} };

vector <pair < pair <int, string>, pair <int, string> > > myTokens, currentToken, stek;
vector <pair <int, string> > vect, myRules;
vector <vector <int> > termInRules, usedRules; 
vector <int> vspom, NNstr;
vector <string> vivod;

int Matr [dim_Matr][dim_Matr]= { {0,0,0,0,0,0,0,0,0,3}, {0,0,2,0,1,0,0,0,0,0}, {3,1,0,2,1,0,0,0,0,0},
   {3,1,2,0,1,0,0,0,0,0}, {3,0,3,3,0,2,2,2,2,0}, {0,0,0,0,2,0,0,0,0,0}, {0,0,0,0,2,0,0,0,0,0},
   {0,0,0,0,2,0,0,0,0,0}, {0,0,0,0,2,0,0,0,0,0}, {1,1,0,0,1,0,0,0,0,0} };
//" " - 0, "<." - 1, "=." - 2, ".>" - 3

string inMatr[] = { "xx", "<.", "=.", ".>" };

int lexer(const char *filename);
int lexer_table();
int fill_data();
int lex_Analizer();
int razbor();
int chains();


//===============================
int lexer(const char *filename) {

   int c, err_symbol;
   char str[256]; 
   int i; 

   ifstream fd (filename);
   if (!fd) {
      cout << "Cannot open the test file" << endl;
      return -1;
   }

   enum states CS = H;

   while(fd) { 

      fd.getline(str,256); //(str,100);
      if (strlen(str)==0) continue;

      i=0;
      c = (int)str[i]; i++;
      CS = H;

      while((i<strlen(str))&&(c>0)) {

         switch(CS) {

            case H: { 
               while((c == ' ') || (c == '\t') || (c == '\n')) {
                  c = str[i]; i++;
               }

               if(((c >= 'A') && (c <= 'Z')) ||
                  ((c >= 'a') && (c <= 'z')) || (c == '_')) {
                  CS = ID;

               }else if(c == ':') {
                  CS = ASGN;

               }else if(c == '"') {
                  CS = LTR;

               }else if(c == '/') {
                  CS = COMMENT;

               }else {
                  CS = DLM;
               }
               break;
            }// case H

            case ASGN: {
               int colon = c;
               c = (int)str[i]; i++;

               if(c == '=') {
                  lexVect.emplace_back(OPER,":=");
                  c = (int)str[i]; i++;
                  CS = H; 
               }else{
                  err_symbol = colon;
                  CS = ERR;
               }
               break;
            }// case ASGN


            case LTR: {
               int size = 0;
               char buf[256];

               if(!strlen(buf)) buf[0]=0;  //clear buffer
               buf[size] = c; size++;

               c = (int)str[i]; i++;   //1st symbol after "

               while( (c > 0) && (!(c==(int)'"')) ) {
                  buf[size] = c; size++;
                  c = str[i]; i++;

                  if (size > len_max) {
                     cout << "Buffer length exceeds " << len_max << " symbols " << endl;
                     return -1;
                  }
               }

               if (c == 0) {
                  cout << "End of string occured " << endl;
                     return -1;
               }

               if (c==(int)'"') {
                  buf[size] = c; size++;
                  buf[size] = '\0'; 
                  
                  lexVect.emplace_back(LITERAL,buf);
                  CS = H; 
               }else{
                  cout << "Literal is not ended" << endl;
                  return -1; 
               }
               c = (int)str[i]; i++;   //1st symbol after loop
               CS = H;
               break;
            }// case LTR (literal)

            case COMMENT: {
               c = (int)str[i]; i++;
               if (c=='/') {
                  while (c > 0) {  //&& (!(c==(int)';')) )
                     c = (int)str[i]; i++;
                  }
               }else{
                  cout << "Unknown character: " << (char)c << endl;
                  return -1;
               }
               CS = H;
               c = (int)str[i]; i++;
               break;
            }//case COMMENT

            case DLM: {
               if(c == ';') {               
                  lexVect.emplace_back(DELIM,";");  //if(c == ';') 

                  if(i<strlen(str)) {
                     c = (int)str[i]; i++;
                  }
                  CS = H;

               }else if((c == '<') || (c == '>') || (c == '=')) {
                  if(c == '<') 
                     lexVect.emplace_back(OPER,"<");
                  else if(c == '>') 
                     lexVect.emplace_back(OPER,">");
                  else 
                     lexVect.emplace_back(OPER,"=");  //if(c == '=')

                  c = str[i]; i++;
                  CS = H;
               }else{
                  err_symbol = c;
                  c = str[i]; i++;
                  CS = ERR;
               } 
               break;
            }// case DLM

            case ERR: {
               cout << "Unknown character: " << (char)c << endl;
               CS = H;
               break;
            }

            case ID: {
               int size = 0;
               char buf[256];

               if(!strlen(buf)) buf[0]=0;  //clear buffer
               buf[size] = c; size++;

               c = str[i]; i++;

               while(((c >= 'A') && (c <= 'Z')) || ((c >= 'a') &&
                  (c <= 'z')) || ((c >= '0') && (c <= '9')) ||
                  (c == '_')) {
                  buf[size] = c; size++;
                  c = str[i]; i++;
               }
               buf[size] = '\0';

               if (size > len_max) {
                  cout << "Buffer length exceeds " << len_max << " symbols " << endl;
                  return -1;
               }

               string sss=buf;
               bool f_is = false;  //keywords[NUM_OF_KWORDS]

               for (int ii = 0; ii < NUM_OF_KWORDS; ii++)
                  if (sss == keywords[ii]) {
                     f_is = true;  // если строка найдена,
                     break; // то выйти из цикла
                  }
            
               if((f_is)&&(c)) {
                  lexVect.emplace_back(KWORD,buf);
               }else{
                  lexVect.emplace_back(IDENT,buf);
               }

               CS = H;
               break;
            } // case ID

         } // switch

      }  //loop while(i<strlen(str))

   } // while

    fd.close();
    return 0; 
} // int lexer(…)


//=================
int lexer_table() {
   int ij = 0, ij0 = 0, it=0, res=0;

   for (int ii = 0; ii < lexVect.size(); ii++) {

      if ((lexVect[ii].first == IDENT)||(lexVect[ii].first == LITERAL)) {

         if (lexIdent.empty()) {
            lexIdent.emplace_back(lexVect[ii].first,lexVect[ii].second);
            ij++;
         }else{
            res = 0;
            for (it = 0; it <= ij; it++) {
               if ((lexVect[ii].first==lexIdent[it].first)&&
                  (lexVect[ii].second==lexIdent[it].second)) {
                  res++;
                  break;
               } 
            }               
            if (res==0) {
               lexIdent.emplace_back(lexVect[ii].first,lexVect[ii].second);
               ij++; 
            }
         } //if...else
      } //if
   } //for loop
   return 0;
}


//===============
int fill_data() {
   int ii = 0, it = 0, ij = 0;
   
   //vector <int> vspom;
   //vector <string> mysymv =  {";", "if", "then", "else", "a", ":=", "<", ">","=",
   //                           "s_first","s_last"};
   // the numbers for symbols: { {";",0},{"if",1},{"then",2},{"else",3},{"a",4},
   //   {":=",5},{"<",6},{">",7},{"=",8},{"s_first",9},{"s_last",10} };

   for (ii = 0; ii < mysymv.size(); ii++) {
      vectSymv.emplace_back(mysymv.at(ii), ii);
   }

   cout << "\n";
   for (ii = 0; ii < vectSymv.size(); ii++) {
      cout << ii+1 << ". ( " << 
      vectSymv[ii].first << " , " << vectSymv[ii].second << " ) " << endl;       
   }

   //vector <pair <string, int> > myRules;
   myRules.emplace_back(1, "E;");
   myRules.emplace_back(2, "if E then E else E");
   myRules.emplace_back(3, "if E then E");
   myRules.emplace_back(4, "a := a");
   myRules.emplace_back(7, "a < a");
   myRules.emplace_back(8, "a > a");
   myRules.emplace_back(9, "a = a");

   cout << "\n" ;
   for (ii = 0; ii < myRules.size(); ii++) {
      cout << ii << ". ( " << 
      myRules[ii].first << " , " << myRules[ii].second << " ) " << endl; 
   } 

   //compare with elements of vector <string> mysymv 
   int pos=-1, pos_curr = 0, count = 0;
   int term[10], rab[10][2];
   string str, substr;

   for (ii = 0; ii < 10; ii++) {
      rab[ii][0] = 0;
      rab[ii][1] = 0;
      term[ii] = 0;
   }

   for (ii = 0; ii < myRules.size(); ii++) {
      str = myRules[ii].second;
      pos = str.length();

      count = 0;
      int ind5=0;

      if (vspom.size()>0)
         vspom.clear();

      for (it = 0; it < (mysymv.size()-1); it++) {
         if ((it==8)&&(ind5>0)) continue;
             //the term ":=" is just found in formula

         pos_curr = pos;
         substr[0] = 0;
         substr.assign(mysymv[it]);
         if (it==(mysymv.size()-2)) 
            substr.assign("E");

         while (pos_curr>0) {
            pos_curr = str.rfind(substr, pos_curr-1);
            if (pos_curr<0) break;

            if (pos_curr>=0) {
               rab[count][0] = pos_curr;
               rab[count][1] = it;
               if (it==(mysymv.size()-2)) 
                  rab[count][1] = -1;  //for symbol E in rules
               term[count] = count;
               count++;                  
               if (it==5) ind5++;
            }
         }
      }
      for (it = 0; it < count; it++) {
         for (int j=it+1; j<count; j++) {
            if (rab[term[it]][0]<rab[term[j]][0]) {
               int ij=term[it];
               term[it]=term[j];
               term[j]=ij;
            }
         }
      }
      vspom.emplace_back(myRules[ii].first);
      vspom.emplace_back(count);

      for (it = 0; it < count; it++) {
         vspom.emplace_back(rab[term[it]][1]);
      }
      termInRules.emplace_back(vspom);
   }

/*
   for (ii = 0; ii < termInRules.size(); ii++) {
      cout << "\n";
      for (it = 0; it < termInRules[ii].size(); it++) {
         cout << termInRules[ii][it] << " ";
         if (it>1) {
            ij = termInRules[ii][it];
            if (ij<0)
               cout << "( E ) " ;
            else
               cout << "( " << mysymv[ij] << " ) " ;
         }
      }
   }
*/

   //int Matr [dim_Matr][dim_Matr];
   //" " - 0, "<." - 1, "=." - 2, ".>" - 3
   //char inMatr[4][2] = { "  ", "<.", "=.", ".>" };

   cout << "\n";
   for (int ii = 0; ii < dim_Matr; ii++) {
      int ij=0; 
      cout << "\n" ;
      for (ij = 0; ij < dim_Matr; ij++) {
         cout << " " << inMatr[Matr[ii][ij]];
      }
   }
   return 0;
}


//==================
int lex_Analizer() {
   int res = 0, res0 = 0;
   int ii = 0, it = 0;
   pair <int, string> para1, para2;
   
   for (ii = 0; ii < lexVect.size(); ii++) {

      if ((lexVect[ii].first == IDENT)||(lexVect[ii].first == LITERAL)) {
         res = 0;
         for (int it = 0; it < lexIdent.size(); it++) {
            if ((lexVect[ii].first==lexIdent[it].first)&&
                (lexVect[ii].second==lexIdent[it].second)) {
               res = it;
               break;
            }
         }
         res0 = 4;
         para1 = make_pair(res0, vectSymv[res0].first);  //"a"
         para2 = make_pair(res, lexVect[ii].second);
         myTokens.emplace_back(para1, para2);

      }else{
         res = 0;
         for (int it = 0; it < vectSymv.size(); it++) {
            if (lexVect[ii].second==vectSymv[it].first) {
               res = it;
               break;
            }
         }
         para1 = make_pair(res, lexVect[ii].second);
         para2 = make_pair(-1, " ");
         myTokens.emplace_back(para1, para2);
      }
   }
   cout << "\n\nmyTokens" ;
   for (ii = 0; ii < myTokens.size(); ii++) {
      para1 = myTokens[ii].first;
      para2 = myTokens[ii].second;
      cout << "\n" << ii << ". ";
      cout << "( " << para1.first << " , " << para1.second << " )  "; 
      if (para2.first>=0)
         cout << "( " << para2.first << " , " << para2.second << " )  "; 
      }
   cout << endl;
   return 0;
}


//============
int razbor() {

   int res = 0, nn = 0, size = 0;
   int ii = 0, it = 0, ij = 0, ir = 0;
   int val = 0, num = 0;
   int code = 0, count = 0, term = 0, tt = 0, rule = 0;
   int seq_size = 0;
   int count0, res0, size_old = 0, rulefound = 0;

   pair <int, string> para1, para2, rab;
   vector <int> vspom;

   nn = 0;
   for (ii = 0; ii < myTokens.size(); ii++) {
      currentToken.emplace_back(myTokens[ii]);
      rab = myTokens[ii].first;

      if (rab.first==0) {   //=0 for ";"
             //current token is filled till ";"
         para1 = make_pair(vectSymv[10].second, vectSymv[10].first);
         para2 = make_pair(-1, " ");
         currentToken.emplace_back(para1, para2);
         nn++;

         //cout << "\n\ncurrent sequence: " << nn << "\n";
         cout << "\n\n";
         for (ij = 0; ij < currentToken.size(); ij++) {
            cout << currentToken[ij].first.second << " "; 
         }
         //cout << endl;
         vspom.emplace_back(nn);

         if (stek.size()>0) stek.clear();

         para1 = make_pair(vectSymv[9].second, vectSymv[9].first);
         para2 = make_pair(-1, " ");
         stek.emplace_back(para1, para2);

         size = currentToken.size();
         it = 0;
         while ((it <= size)&&(size>0)) {

            para1 = stek[stek.size()-1].first;
            para2 = currentToken[it].first;

            //for (ij = 0; ij < stek.size(); ij++) {
            //   cout << stek[ij].first.second << " "; 
            //}

            if ( ((stek.size()==2)&&(stek[stek.size()-1].first.second=="E")) &&
                 (currentToken[it].first.first==dim_Matr) ) {
               cout << "\nSequence NN " << nn << " is analyzed. All is OK.";

               currentToken.clear();
               cout << "\nUsed rules (sequence " << vspom[0] << ") :";
               for (ij = 1; ij < vspom.size(); ij++) {
                  cout << " " << vspom[ij];
                  cout << "(NN " << myRules[vspom[ij]].first << ")";
                  if (ij < (vspom.size() - 1))
                     cout << ",";
               }
               usedRules.emplace_back(vspom);
               if (vspom.size()>0) vspom.clear();
               break;
            }

            res = stek.size();
            if (res==0) {
               cout << "\nError: size=0" <<  "\n" ;
               return(-1);
            }

            //to check on last terminal symbol in stek
            term = res-1;  //here res = stek.size();
            num = stek[term].first.first;

            while(( stek[term].first.first < 0)&&(term>=0)) {
               term--;
               num = stek[term].first.first;
            }

            if (currentToken[it].first.first==dim_Matr) {
               val = currentToken[it].first.first - 1; 
            }else{ 
               val = currentToken[it].first.first;
            }
            //cout << "\nnum = " << num << " val = " << val;
            val = Matr[num][val];
            //cout << " matrix value = " << val;

            if ((val==1)||(val==2)) {
               code = 1; //code=1 - sdvigue; code=2 - svertka;
               stek.emplace_back(currentToken[it]);
               it++;
      
            }else if (val==3){
               code = 2; //code=1 - sdvigue; code=2 - svertka;

               for (ir = 0; ir < termInRules.size(); ir++) {
                  count =  termInRules[ir][1];
                  res = 0;
                  tt = stek.size()-1;

                  for (ij = 0; ij < count; ij++) {
                     if (stek[tt].first.first==termInRules[ir][ij+2]) {
                        res++;
                        tt--;
                        if (tt<0) break;
                     }else
                        break;
                  }
                  if (res==count) {
                     rule = ir; 
                     break;
                  }
               }
               if (res==count) {
                  //cout << "\nFound roule NN " << rule << ", count= " << count;

                  for (ij = 0; ij < count; ij++) {
                     if (stek.size()>0) {
                        stek.pop_back();
                     }else{
                        cout << "\nError: stek is empty";
                        return (-1);
                     } 
                  }
                  para2 = make_pair(-1, "E");
                  stek.emplace_back(para2, para2);

                  vspom.emplace_back(rule);

                  //One more checking is needed;
                  rulefound = 0; 
                  size_old = stek.size();
                  for (term = 0; term < stek.size(); term++) { 

                     if ((term>0)&&(rulefound==0)) break;   //rules 1,2 are not found
                     if ( ((term>0)&&(rulefound==1))&&(size_old==stek.size()) ) 
                        break;    //all inclusions of rules 1,2 are just found

                     for (ir = 1; ir < 2; ir++) {
                        count0 =  termInRules[ir][1];
                        res0 = 0;
                        tt = stek.size()-1;

                        for (ij = 0; ij < count0; ij++) {
                           if (stek[tt].first.first==termInRules[ir][ij+2]) {
                              res0++;
                              tt--;
                              if (tt<0) break;
                           }else break;
                        }
                        if (res0==count0) {
                           rulefound = 1;
                           rule = ir; 
                           size_old = stek.size();

                           for (ij = 0; ij < count0; ij++) {
                              if (stek.size()>0) 
                                 stek.pop_back();
                           }
                           para2 = make_pair(-1, "E");
                           stek.emplace_back(para2, para2);
                           vspom.emplace_back(rule);

                           //for (ij = 0; ij < stek.size(); ij++) {
                           //   cout << stek[ij].first.second << " "; 
                           //}
                           break;
                        }
                     if (rulefound==1) break;

                     }  //end of loop for (ir = 0; ir < 2; ir++) 
                  }  //end of loop for (term = 0; term < stek.size(); term++)
               }
            }else{
               cout << "\nSyntax error in sequence " << nn;
               cout << "\nWrong value of matrix" <<  "\n" ;

               for (ij = 0; ij < currentToken.size(); ij++) {
                  cout << currentToken[ij].first.second << " "; 
         }
         cout << endl;

               return(-1);
            }
         }  //while
         
      }  //end of   if (myTokens[ii].first==0)
   } //loop for 


   cout << endl;
   for (ii = 0; ii < usedRules.size(); ii++) {
      cout << "\nUsed rules in sequence " << termInRules[ii][0] << " : ";

      for (it = 1; it < usedRules[ii].size(); it++) {
            ir = usedRules[ii][it];
            cout << ir; //usedRules[ii][it];
            cout << "(NN " << myRules[ir].first << ")"; 
            if (it < (usedRules[ii].size() - 1))
               cout << ", "; 
      }
   }
   return 0;
}


//============
int chains() {

   int ii = 0, it = 0, pos_curr;
   int nn = 0, steps = 0;

   //vector <vector <int> > usedRules
   //vector <string> vivod;

   string form, form0;
   
   cout << endl;
   nn = 0;
   for (ii = 0; ii < usedRules.size(); ii++) {
      vivod.emplace_back("E");
      nn++;
      steps = usedRules[ii].size() - 1;
      form.clear();
      if (ii>0)
         NNstr.emplace_back(nn-1);

      for (it = steps; it > 0; it--) {

         if (it==steps) {
            form = myRules[usedRules[ii][it]].second;
            vivod.emplace_back(form);
            nn++;
         }else{
            pos_curr = form.rfind("E", form.length());
            if (pos_curr<0) {
               cout << "\nThere are some errors";
               break;      
            }
            form0.clear();
            form0 = myRules[usedRules[ii][it]].second;
            form.replace(pos_curr, 1, form0, 0, form0.length());
            vivod.emplace_back(form);
            nn++;
         }
      }
   }
   it = 0;
   nn = NNstr[it];
   for (ii = 0; ii < vivod.size(); ii++) {
      if (ii==nn) {
         cout << "\n";
         it++; 
         nn = NNstr[it];
      }
      cout << "\n" << vivod[ii];
   }

   cout << endl;
   return 0;
}


//==========================
int main() {

   lexer(filename);
   //lexer("mytest.txt");

   for (int ii = 0; ii < lexVect.size(); ii++) {
      cout << ii << ". ( " << 
      lex_names[lexVect[ii].first] << ", "<<lexVect[ii].second<<" ) "<<endl;       
   }

   //for(auto word:lexVect) {
   //   std::cout<< "( " << 
   //               lex_names[word.first] << ", "<<word.second<<" ) "<<std::endl;
   //std::cout<<"("<<word.first<<","<<word.second<<")"<<std::endl;
   //}

   cout << endl; 
   lexer_table();

   cout << "\nIdentifiers table" << endl; 
   for (int ii = 0; ii < lexIdent.size(); ii++) {
      cout << ii << ". ( " << 
      lex_names[lexIdent[ii].first] << ", "<<lexIdent[ii].second<<" ) "<<endl;       
   } 

   fill_data();
   lex_Analizer();
   razbor();
   chains();

return 0;
}

