

#include <cstdlib>
#include <cstddef>
#include <cstdio>
#include <cstring>
using namespace std;

#include<iostream>
#include <fstream>

#include<vector>

#define NUM_OF_KWORDS 3
#define len_max 32

const string keywords[NUM_OF_KWORDS] = {"if", "then", "else"};
//char symv[] = {';', '<', '>', ':', '=', '\"', '/'}; ':='};

const char *filename = "mytest.txt";

enum states {H, ID, LTR, ASGN, DLM, COMMENT, ERR};
enum tok_names {KWORD, IDENT, LITERAL, OPER, DELIM};
const string lex_names[] = {"KWORD", "IDENT", "LITERAL", "OPER", "DELIM"};

vector <pair <int, string> > lexVect;
vector <pair <int, string> > lexIdent;

int lexer(char *filename);
int lexer_table();
//int comp_symv(char c);


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

      fd.getline(str,256);  //(str,100);
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
                  cout << "Unknown character: " << (char)c << "\n" << endl;
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

/*
//=====================
int comp_symv(char c) {
   int ij = -1; 
   for (int ii = 0; ii < sizeof(symv); ii++) {
      if (c==(int)symv[ii]) {
         ij=ii;
         break;
      } 
   }
   return ij;
}
*/


//==========================
int main() {
   lexer(filename);
   //lexer("mytest.txt");

   for (int ii = 0; ii < lexVect.size(); ii++) {
      cout << ii+1 << ". ( " << 
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
      cout << ii+1 << ". ( " << 
      lex_names[lexIdent[ii].first] << ", "<<lexIdent[ii].second<<" ) "<<endl;       
   } 
return 0;
}

