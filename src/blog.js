import "./styles.css";
import "./main.js";

import $ from 'jquery';

//Pagination for blog
var pageIndex = 0
var numberOfPages = 0
var blogCardsGrid = document.getElementById("blog-cards-grid");
var paginationDiv = document.getElementById("page-numbers");
document.onload($("#blog-cards-grid").load("./blog-pages/1.html"));
// get value for number of pages from the pagination bar html. The last
// button contains the numberOfPages. based off of the haskell generated html
// Not true for small pagination though
var prevButton = document.getElementById("prev-page");
var nextButton = document.getElementById("next-page");

prevButton.addEventListener("click", prevButtonAction);
nextButton.addEventListener("click", nextButtonAction);

function prevButtonAction () {
  if (pageIndex > 0) {
    // IF the the viewport is small then there's only one page number
    // To change numbers when the viewport is small we just change the number
    // because there's only one slot.
    if (window.matchMedia("screen and (min-width: 768px)").matches) {
    // When the viewport is big it's a bit more difficult because the numbers have to "scroll"
    // back and forth. Wait, the numbers only have to scroll back and forth if there are more than
    // 5 pages. So six pages is where the big ellipsed navbar is needed and scrolling is need to
    // navigate that.
      if (numberOfPages > 5) {
        // IF on the last page
        if (pageIndex === numberOfPages - 1) {
          paginationDiv
          .lastChild
          .classList
          .remove(["bg-blue","text-white"]);

          paginationDiv
          .lastChild
          .previousSibling
          .previousSibling
          .classList
          .add(["bg-blue","text-white"])

          if (numberOfPages) {}
        }
        // How will scrolling work?
        // Let's say we have 1 ... 2 3 4 ... 6
        // Only the middle need to scroll! The first and last pages stay the same
        // so it's like a moving window of 3 values.
        // The user hits next: 1 ... 3 4 5 ... 6
        // Scrolling needed here.

      } else {
        paginationDiv
        .children[pageIndex]
        .classList
        .remove(["bg-blue","text-white"]);

        pageIndex--;

        //Simple scrolling here.
        paginationDiv
        .children[pageIndex]
        .classList
        .add(["bg-blue","text-white"]);
      }
    } else {
      // Here is number change for small navbar. Just change the number inside the single slot.
      // decrement page index
      pageIndex--;
      paginationDiv.lastChild.innerHTML = pageIndex + 1;
    }


    // remove blue-bg from current page number
    paginationDiv
    .children[pageIndex]
    .classList
    .remove(["bg-blue","text-white"]);

    // add blue-bg to prev page number
    paginationDiv
    .children[pageIndex]
    .classList
    .add(["bg-blue","text-white"])

    var reader = new FileReader();
    blogCardsGrid.innerHTML =
      reader
      .readAsText("./blog-pages/" + Number.toString(pageIndex + 1) + ".html");
  }
}

function nextButtonAction () {
  if (pageIndex > 0) {
    // remove blue-bg from current page number
    paginationDiv
    .children[pageIndex]
    .classList
    .remove(["bg-blue","text-white"]);

    // increment page index
    pageIndex++;

    // add blue-bg to prev page number
    paginationDiv
    .children[pageIndex]
    .classList
    .add(["bg-blue","text-white"])

    var reader = new FileReader();
    blogCardsGrid.innerHTML =
      reader
      .readAsText("./blog-pages/" + Number.toString(pageIndex + 1) + ".html");
  }
}
