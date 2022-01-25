public class WordCounter {
...
  void countWords() {
    loadData();
    processData();
    calculateTermFrequency();
    calculateTermDocumentFrequency();
    saveCountData();
  }
  abstract void processData();

}

public class LowerCaseWordCounter {
  void processData() {
    removeUpperCase();
  }
}
