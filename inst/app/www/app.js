(function () {
  "use strict";

  function setCookie(name, value, days) {
    var expires = "";
    if (days) {
      var date = new Date();
      date.setTime(date.getTime() + days * 24 * 60 * 60 * 1000);
      expires = "; expires=" + date.toUTCString();
    }
    var secureFlag = window.location && window.location.protocol === "https:" ? "; Secure" : "";
    document.cookie =
      name +
      "=" +
      encodeURIComponent(value || "") +
      expires +
      "; path=/; SameSite=Lax" +
      secureFlag;
  }

  function getCookie(name) {
    var nameEQ = name + "=";
    var parts = document.cookie.split(";");
    for (var i = 0; i < parts.length; i += 1) {
      var c = parts[i];
      while (c.charAt(0) === " ") c = c.substring(1, c.length);
      if (c.indexOf(nameEQ) === 0) {
        return decodeURIComponent(c.substring(nameEQ.length, c.length));
      }
    }
    return null;
  }

  function eraseCookie(name) {
    var secureFlag = window.location && window.location.protocol === "https:" ? "; Secure" : "";
    document.cookie = name + "=; Max-Age=-1; path=/; SameSite=Lax" + secureFlag;
  }

  function sendRememberTokenRevoke(token) {
    if (!token) return;
    sendToShiny("remembered_token_revoke", token, 30);
  }

  function sendToShiny(inputId, value, retries) {
    var remaining = typeof retries === "number" ? retries : 30;
    if (window.Shiny && typeof window.Shiny.setInputValue === "function") {
      window.Shiny.setInputValue(inputId, value, { priority: "event" });
      return;
    }
    if (remaining <= 0) return;
    window.setTimeout(function () {
      sendToShiny(inputId, value, remaining - 1);
    }, 200);
  }

  function registerMessageHandler(applyServerTheme, retries) {
    var remaining = typeof retries === "number" ? retries : 30;
    if (window.Shiny && typeof window.Shiny.addCustomMessageHandler === "function") {
      window.Shiny.addCustomMessageHandler("auth_cookie", function (msg) {
        if (!msg || !msg.action) return;
        if (msg.action === "set") {
          var consent = getCookie("archaeodash_cookie_consent");
          if (consent === "accepted" && msg.token) {
            setCookie("archaeodash_auth_token", msg.token, 30);
            sendToShiny("remembered_token", msg.token, 30);
          }
        } else if (msg.action === "clear") {
          var existingToken = getCookie("archaeodash_auth_token");
          if (existingToken) {
            sendRememberTokenRevoke(existingToken);
          }
          eraseCookie("archaeodash_auth_token");
        }
      });
      window.Shiny.addCustomMessageHandler("theme_preference", function (msg) {
        if (!msg || !msg.theme || typeof applyServerTheme !== "function") return;
        applyServerTheme(msg.theme);
      });
      return;
    }
    if (remaining <= 0) return;
    window.setTimeout(function () {
      registerMessageHandler(applyServerTheme, remaining - 1);
    }, 200);
  }

  function closeColvisCollection() {
    var collections = document.querySelectorAll("div.dt-button-collection");
    for (var i = 0; i < collections.length; i += 1) {
      collections[i].style.display = "none";
      collections[i].classList.remove("show");
    }
    var backgrounds = document.querySelectorAll("div.dt-button-background");
    for (var j = 0; j < backgrounds.length; j += 1) {
      if (backgrounds[j] && backgrounds[j].parentNode) {
        backgrounds[j].parentNode.removeChild(backgrounds[j]);
      }
    }
  }

  function bindColvisCloseBehavior() {
    document.addEventListener(
      "click",
      function (e) {
        var clickedInsideMenu = e.target.closest("div.dt-button-collection");
        var clickedColvisButton = e.target.closest("button.buttons-colvis, a.buttons-colvis");
        if (!clickedInsideMenu && !clickedColvisButton) {
          closeColvisCollection();
        }
      },
      true
    );

    document.addEventListener("keydown", function (e) {
      if (e.key === "Escape") {
        closeColvisCollection();
      }
    });
  }

  function getStoredTheme() {
    try {
      var stored = window.localStorage.getItem("archaeodash_theme");
      if (stored === "simple" || stored === "dark" || stored === "light") return stored;
    } catch (e) {}
    return "simple";
  }

  function setStoredTheme(mode) {
    try {
      window.localStorage.setItem("archaeodash_theme", mode);
    } catch (e) {}
  }

  function normalizeTheme(mode) {
    if (mode === "dark" || mode === "light" || mode === "simple") return mode;
    return "simple";
  }

  function applyTheme(mode, selectInputEl) {
    var normalized = normalizeTheme(mode);
    if (document.body) {
      document.body.classList.remove("theme-simple", "theme-light", "theme-dark");
      document.body.classList.add("theme-" + normalized);
    }
    if (selectInputEl && selectInputEl.value !== normalized) {
      selectInputEl.value = normalized;
    }
    return normalized;
  }

  function init() {
    try {
      var toggleBtn = document.getElementById("toggleSidebar");
      var showSidebarBtn = document.getElementById("showSidebarBtn");
      var themeSelect = document.getElementById("themeSelect");
      var sideCol = document.getElementById("sidePanelCol");
      var mainCol = document.getElementById("mainPanelCol");
      var banner = document.getElementById("cookieBanner");
      var acceptBtn = document.getElementById("acceptCookies");
      var declineBtn = document.getElementById("declineCookies");
      var privacyLink = document.getElementById("privacyPolicyLink");

      if (toggleBtn && sideCol && mainCol) {
        toggleBtn.addEventListener("click", function () {
          sideCol.style.display = "none";
          mainCol.classList.remove("col-sm-9", "col-md-9");
          mainCol.classList.add("col-sm-12", "col-md-12");
          toggleBtn.setAttribute("data-collapsed", "true");
          if (showSidebarBtn) {
            showSidebarBtn.style.display = "";
          }
        });
      }

      if (showSidebarBtn && toggleBtn && sideCol && mainCol) {
        showSidebarBtn.addEventListener("click", function () {
          sideCol.style.display = "";
          mainCol.classList.remove("col-sm-12", "col-md-12");
          mainCol.classList.add("col-sm-9", "col-md-9");
          toggleBtn.setAttribute("data-collapsed", "false");
          showSidebarBtn.style.display = "none";
        });
      }

      var activeTheme = applyTheme(getStoredTheme(), themeSelect);
      sendToShiny("theme_preference_set", activeTheme, 30);

      if (themeSelect) {
        themeSelect.addEventListener("change", function () {
          activeTheme = applyTheme(themeSelect.value, themeSelect);
          setStoredTheme(activeTheme);
          sendToShiny("theme_preference_set", activeTheme, 30);
        });
      } else {
        applyTheme(activeTheme, null);
      }

      var consent = getCookie("archaeodash_cookie_consent");
      if (banner) {
        banner.style.display = consent ? "none" : "block";
      }

      if (consent === "accepted") {
        var rememberedToken = getCookie("archaeodash_auth_token");
        if (rememberedToken) {
          sendToShiny("remembered_token", rememberedToken, 30);
        }
      }

      if (acceptBtn) {
        acceptBtn.addEventListener("click", function () {
          setCookie("archaeodash_cookie_consent", "accepted", 180);
          if (banner) banner.style.display = "none";
          sendToShiny("cookie_consent", "accepted", 30);
        });
      }

      if (declineBtn) {
        declineBtn.addEventListener("click", function () {
          setCookie("archaeodash_cookie_consent", "declined", 180);
          var existingToken = getCookie("archaeodash_auth_token");
          if (existingToken) {
            sendRememberTokenRevoke(existingToken);
          }
          eraseCookie("archaeodash_auth_token");
          if (banner) banner.style.display = "none";
          sendToShiny("cookie_consent", "declined", 30);
        });
      }

      if (privacyLink) {
        privacyLink.addEventListener("click", function (e) {
          e.preventDefault();
          sendToShiny("open_privacy_policy", Date.now(), 30);
        });
      }

      bindColvisCloseBehavior();
      registerMessageHandler(function (theme) {
        activeTheme = applyTheme(theme, themeSelect);
        setStoredTheme(activeTheme);
      }, 30);
    } catch (err) {
      if (window.console && console.error) {
        console.error("app.js init failed", err);
      }
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
})();
