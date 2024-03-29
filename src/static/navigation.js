class AtbNavigation extends HTMLElement {
    constructor() {
        super();
        this.mql = window.matchMedia('(max-width: 950px)');
    }

    connectedCallback() {
        this.alwaysOpen = !this.mql.matches;
        this.setListeners();
        this.setOptions();
    }

    toggleOpen() {
        if (!this.alwaysOpen) {
            this.open = !this.open;
        }
    }

    disconnectedCallback() {
        this.removeListeners();
    }

    static get observedAttributes() {
        return ['open', 'always-open'];
    }

    attributeChangedCallback(name, oldValue, newValue) {
        this.setOptions();
    }

    onKeyDown = (e) => {
        if (e.key === 'Escape' && this.open) {
            this.open = false;
        }
    };

    onElementClick = (e) => {
        const name = e.target.nodeName;
        if (name == 'A' || name == 'ATB-NAV') {
            this.open = false;
        }
    };

    onMediaChange = (e) => {
        this.alwaysOpen = !e.matches;
    };

    setListeners() {
        this.parentElement.addEventListener('click', this.onElementClick);
        document.body.addEventListener('keydown', this.onKeyDown);

        if (typeof this.mql.onchange !== 'undefined') {
            this.mql.addEventListener('change', this.onMediaChange);
        } else if (typeof this.mql.addListener !== 'undefined') {
            // Fallback for older Safari (pre 14)
            this.mql.addListener(this.onMediaChange);
        }
    }

    removeListeners() {
        if (this.parentElement) {
            this.parentElement.removeEventListener(
                'click',
                this.onElementClick
            );
        }

        if (document.body) {
            document.body.removeEventListener('keydown', this.onKeyDown);
        }

        if (typeof this.mql.onchange !== 'undefined') {
            this.mql.removeEventListener('change', this.onMediaChange);
        } else if (typeof this.mql.removeListener !== 'undefined') {
            // Fallback for older Safari (pre 14)
            this.mql.removeListener(this.onMediaChange);
        }
    }

    setOptions() {
        if (this.alwaysOpen) {
            this.setAttribute('aria-disabled', 'true');
            this.removeAttribute('inert');
            this.contentContainer.removeAttribute('inert');
            this.setAttribute('aria-disabled', 'false');

            this.dispatchEvent(new CustomEvent('change', { open: false }));

            return;
        }

        if (!this.open) {
            this.setAttribute('aria-disabled', 'true');
            this.setAttribute('inert', 'true');
            document.body.classList.remove('menuOpen');

            if (this.contentContainer) {
                this.contentContainer.removeAttribute('inert');
                this.contentContainer.setAttribute('tabIndex', '0');
                this.contentContainer.setAttribute('aria-disabled', 'false');
            }
        } else {
            this.setAttribute('aria-disabled', 'false');
            this.removeAttribute('inert');
            document.body.classList.add('menuOpen');

            if (this.contentContainer) {
                this.contentContainer.setAttribute('inert', 'true');
                this.contentContainer.setAttribute('tabIndex', '-1');
                this.contentContainer.setAttribute('aria-disabled', 'true');
            }

            this.setFirstInteractiveFocus();
        }
    }

    setFirstInteractiveFocus() {
        const element = this.querySelector('a,button');

        if (element) {
            element.focus();
        }
    }

    get contentContainer() {
        return document.body.querySelector(
            '.' + this.getAttribute('content-class')
        );
    }

    get open() {
        return this.hasAttribute('open');
    }

    set open(val) {
        if (val) {
            this.setAttribute('open', '');
        } else {
            this.removeAttribute('open');
        }

        this.dispatchEvent(new CustomEvent('change', { open: val }));
    }

    get alwaysOpen() {
        return this.hasAttribute('always-open');
    }

    set alwaysOpen(val) {
        if (val) {
            this.setAttribute('always-open', '');
            this.dispatchEvent(new CustomEvent('change', { open: false }));
        } else {
            this.removeAttribute('always-open');
        }
    }
}

window.customElements.define('atb-nav', AtbNavigation);
