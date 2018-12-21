#include <Rcpp.h>
#include <unordered_map>
#include <string>
#include <iostream>
using namespace std;
using namespace Rcpp;
#define MIN(X,Y) ((X<Y)?X:Y)
unordered_map<string,int> MutateHashTable(Rcpp::CharacterVector tokenList, unordered_map<string,int> prevHashTable, unordered_map<string,int> monoHashTable, int ngram, double threshold);

unordered_map<string,int> GetMonoGram(Rcpp::CharacterVector tokenList, double threshold){
    int maxTokens = (int)1e5;
    unordered_map <string, int> hashTable;
    hashTable.reserve(maxTokens);
    std::unordered_map <string,int>::iterator myFind;
    for(int i=0; i< tokenList.size(); i++){
        string myString = Rcpp::as<string>(tokenList[i]);
        myFind  = hashTable.find(myString);
        if(myFind != hashTable.end()){
            myFind->second++;
        }
        else
            hashTable[myString] = 1;
    }
    
    int i=0;
    int intThres = threshold*tokenList.size();
    // cout << intThres << endl;
    for(unordered_map <string, int>::iterator it=hashTable.begin(); it != hashTable.end(); i++){
        if(it->second < intThres)
            it  = hashTable.erase(it);  
        else
            it++;
    }
    return hashTable;
}

// [[Rcpp::export]]

Rcpp::List GetMultigram(Rcpp::CharacterVector tokenList, double threshold, int maxNgram=5){
    unordered_map <string,int> *hashTables = new unordered_map<string,int>[maxNgram];
    
    hashTables[0] = GetMonoGram(tokenList, threshold);
    
    for(int ngram=2; ngram<=maxNgram; ngram++){
        hashTables[ngram-1] = MutateHashTable(tokenList, hashTables[ngram-2], hashTables[0], ngram, threshold);
    }
    
    Rcpp::List allNGram; 
    for(int ngram=1; ngram<=maxNgram; ngram++){
        CharacterVector tokenNames(hashTables[ngram-1].size());
        NumericVector tokenCounts(hashTables[ngram-1].size());
        
        int i=0;
        for(unordered_map<string,int>::iterator it=hashTables[ngram-1].begin(); it!=hashTables[ngram-1].end(); it++, i++){
            tokenNames[i] = it->first;
            tokenCounts[i] = it->second;
        }
        string curStr = to_string(ngram);
        allNGram[curStr] = Rcpp::List::create(
            Rcpp::Named("count") = tokenCounts,
            Rcpp::Named("phrase") = tokenNames
        );
    }
    return allNGram;
}

unordered_map<string,int> MutateHashTable(Rcpp::CharacterVector tokenList, unordered_map<string,int> prevHashTable, unordered_map<string, int> monoHashTable, int ngram, double threshold){
    int nTokens = tokenList.size();
    int maxTokens = (int)1e5;
    unordered_map <string, int> newHashTable;
    newHashTable.reserve(maxTokens);
    
    std::unordered_map<string,int>::iterator prevFindA, prevFindB, newFind;
    string* ringBuffer = new string[ngram];
    
    for(int i=0; i<ngram-1; i++) ringBuffer[i] = Rcpp::as<string>(tokenList[i]);
    
    for(int i=ngram-1; i<tokenList.size(); i++){
        string newToken, prevTokenA, prevTokenB;
        prevTokenB = Rcpp::as<string>(tokenList[i]);
        ringBuffer[i%ngram] = prevTokenB;
        newToken = ringBuffer[(i-ngram+1)%ngram];
        prevTokenA = ringBuffer[(i-ngram+1)%ngram];
        for(int j=i-ngram+2; j<=i; j++){
            newToken += " " + ringBuffer[j%ngram];
            if(j != i)
                prevTokenA += " " + ringBuffer[j%ngram];
        }
        prevFindA = prevHashTable.find(prevTokenA);
        prevFindB = monoHashTable.find(prevTokenB);
        // if(ngram == 3 && i<100)
            // cout << "A: " << prevTokenA << "\nB: " << prevTokenB << endl;
        if(prevFindA != prevHashTable.end() && prevFindB != monoHashTable.end()){
            double score  = MIN(prevFindA->second, prevFindB->second)/(double)nTokens;
            // cout << "score: " << score << endl;
            if(score > threshold){
                newFind = newHashTable.find(newToken);
                if(newFind != newHashTable.end())
                    newFind->second++;
                else
                    newHashTable[newToken] = 1;
            }
        }
        
    }
    
    int intThres = threshold*nTokens;
    int i=0;
    for(unordered_map <string, int>::iterator it=newHashTable.begin(); it != newHashTable.end(); i++){
        if(it->second < intThres){
            it  = newHashTable.erase(it);  
        } 
        else{
            it++;
        }
    }
    
    
    delete[] ringBuffer;
    return newHashTable;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

