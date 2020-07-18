import "./styles.css";
import "./main.js";

function checkDarkMode() {
  return window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;
}

if (true) {
  document.documentElement.classList.add('mode-dark');
} else {
  document.documentElement.classList.remove('mode-dark');
}
