



  function findSimilarWords(wordVecs, n, word) {
    if (!wordVecs.hasOwnProperty(word)) {
      return [false, word];
    }

    return getNClosestMatches(
      wordVecs, n, wordVecs[word]
    );
  }

  function getNClosestMatches(wordVecs, n, vec) {
    var sims = [];
    for (var word in wordVecs) {
      var sim = getCosSim(vec, wordVecs[word]);
      sims.push([word, sim]);
    }
    sims.sort(function(a, b) {
      return b[1] - a[1]; 
    });
    return sims.slice(0, n);
  }

  /********************
   * helper functions */
  function getCosSim(f1, f2) {
    return Math.abs(f1.reduce(function(sum, a, idx) {
      return sum + a*f2[idx];
    }, 0)/(mag(f1)*mag(f2))); //magnitude is 1 for all feature vectors
  }
  
  function mag(a) {
    return Math.sqrt(a.reduce(function(sum, val) {
      return sum + val*val;  
    }, 0));
  }