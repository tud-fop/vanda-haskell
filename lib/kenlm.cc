#include "lm/model.hh"
#include "lm/virtual_interface.hh"
#include <iostream>
#include <string>

using namespace lm::ngram;

extern "C" {
	
	extern TrieModel* loadModel(const char* path) {
		return new TrieModel(path);
	}
	
	extern int order(const TrieModel* model) {
		return model->Order();
	}
	
	extern int indexWord(const TrieModel* model, const char* word) {
		const std::string s(word);
		const SortedVocabulary &vocab = model->GetVocabulary();
		return vocab.Index(s);
	}
	extern State beginSentenceState(const TrieModel* model) {
		return model->BeginSentenceState();
	}

	extern State nullContextState(const TrieModel* model) {
		return model->NullContextState();
	}

	extern float lookup(const TrieModel* model, const char* sentence, const State* startState) {
		const SortedVocabulary &vocab = model->GetVocabulary();
		State oldState(*startState);
		State newState;
		std::string word;
		std::stringstream ss(sentence);
		float sum = 0;
		while (ss >> word) {
			sum = sum + model->Score(oldState, vocab.Index(word), newState);
			oldState = newState;
		}
		return sum;
	}
	
	extern float lookupInt(const TrieModel* model, const int* sentence, const int lSentence, const State* startState) {
		State oldState(*startState);
		State newState;
		float sum = 0;
		int i;
		for (i = 0; i < lSentence; i++) {
			sum = sum + model->Score(oldState, sentence[i], newState);
			oldState = newState;
		}
		return sum;
	}
	
	extern float score(const TrieModel* model, const char* sentence) {
		State startState(model->BeginSentenceState());
		return lookup(model, sentence, &startState);
	}
	
	extern float scoreInt(const TrieModel* model, const int* sentence, const int lSentence) {
		State startState(model->BeginSentenceState());
		return lookupInt(model, sentence, lSentence, &startState);
	}

}