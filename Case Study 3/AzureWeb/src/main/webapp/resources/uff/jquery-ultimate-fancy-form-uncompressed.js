var jQueryUFF = {

  formObject : {},

  Init : function(formObject) {
    if(formObject.hasClass('dispatched'))
      return;

    formObject.addClass('dispatched');

    this.formObject = formObject;
    this.formObject.data('uff', this);

    var objectInstance = this;

    jQuery.each(this.Helpers, function(key, helper) {
      helper.Init();
    });

    jQuery.each(this.Handlers, function(key, helper) {
      helper.Init(objectInstance.formObject);
    });
  },

  Handlers : {

    StoryBoxController : {

      controller : {},
      formObject : {},

      Init : function(formObject) {
        this.formObject   = formObject;
        this.controller   = this.formObject.data('uff');

        this.controller.Libraries.EventManager.listenEvent(
            'stepControllerStepDisplay',
            this,
            '_eventListenerStepDisplay'
        );
      },

      _eventListenerStepDisplay : function(stepObject) {
        var storyBoxInstance = this.controller.Libraries.factory(this.controller.Libraries.StoryBox);

        storyBoxInstance.Init(stepObject);
      }

    },

    StoryTaleController : {

      controller : {},
      formObject : {},

      Init : function(formObject) {
        this.formObject   = formObject;
        this.controller   = this.formObject.data('uff');

        this.controller.Libraries.EventManager.listenEvent(
            'stepControllerStepDisplay',
            this,
            '_eventListenerStepDisplay'
        );
      },

      _eventListenerStepDisplay : function(stepObject) {
        var storyBoxInstance = this.controller.Libraries.factory(this.controller.Libraries.StoryTale);

        storyBoxInstance.Init(stepObject);
      }

    },

    StepControllerMirror : {
      controller                     : {},
      formObject                     : {},

      Init : function(formObject) {
        var objectInstance = this;

        this.formObject                = formObject;
        this.controller                = this.formObject.data('uff');

        jQuery.each(this._handlers, function(key, helper) {
          helper.Init(objectInstance.formObject);
        });

        this.controller.Libraries.EventManager.listenEvent(
            'stepControllerStepDisplay',
            this,
            '_eventListenerStepDisplay'
        );
      },

      _handlers : {

        mirror : {
          formObject     : {},
          stepIdentifier : '[data-step-mirror]',
          stepObject     : {},

          Init : function(formObject) {
            this.formObject = formObject;
            this.stepObject = this.formObject.find(this.stepIdentifier);
            this.stepObject.hide();
          },

          _eventListenerStepDisplay : function(stepObject) {
            var currentMirrorObject = this.stepObject.eq(stepObject.attr('data-step-number'));

            this.stepObject.not(currentMirrorObject).slideUp();
            currentMirrorObject.slideDown();
          }

        },

        mirrorCascading : {
          formObject     : {},
          stepIdentifier : '[data-step-mirror-cascading]',
          stepObject     : {},

          Init : function(formObject) {
            this.formObject = formObject;
            this.stepObject = this.formObject.find(this.stepIdentifier);
            this.stepObject.hide();
          },

          _eventListenerStepDisplay : function(stepObject) {
            this.stepObject.slice(0, (parseInt(stepObject.attr('data-step-number'), 10) + 1)).slideDown();
          }

        }

      },

      _eventListenerStepDisplay : function(stepObject) {
        var objectInstance = this;

        jQuery.each(this._handlers, function(key, helper) {
          helper._eventListenerStepDisplay(stepObject);
        });
      }
    },

    StepController : {

      onStepDisplayEvent     : 'stepControllerStepDisplay',
      beforeStepDisplayEvent : 'stepControllerBeforeStepDisplay',
      beforeStepFinishEvent  : 'stepControllerBeforeStepFinish',

      isOkay                : true,

      stepIdentifier             : '[data-step]',
      stepPreviousIdentifier     : '[data-step-previous]',
      stepNextIdentifier         : '[data-step-next]',
      stepFinishIdentifier       : '[data-step-finish]',
      stepDisplayIdentifier      : '[data-step-display]',
      stepForceDisplayIdentifier : '[data-step-force]',
      stepDisplayNumber          : 'data-step-display',
      formObject                 : {},
      stepObjects                : {},
      stepCount                  : 0,
      currentStep                : 1,
      currentStepObject          : {},

      stepDisplayMethodAttribute: 'data-step-method',
      defaultStepDisplayMethod  : 'classic',
      stepDisplayMethods        : {
        'classic' : function(objectInstance, spartanDisplay) {
          if(spartanDisplay) {
            objectInstance.stepObjects.not(objectInstance.currentStepObject).hide();
            objectInstance.currentStepObject.show(function(){
              objectInstance.formObject.data('uff').Libraries.EventManager.triggerEvent(
                  objectInstance.onStepDisplayEvent,
                  objectInstance.currentStepObject
              );
            });
          } else {
            objectInstance.stepObjects.not(objectInstance.currentStepObject).hide();
            objectInstance.currentStepObject.fadeIn('slow', function(){
              objectInstance.formObject.data('uff').Libraries.EventManager.triggerEvent(
                  objectInstance.onStepDisplayEvent,
                  objectInstance.currentStepObject
              );
            });
          }
        },
        'sliding' : function(objectInstance, spartanDisplay) {
          if(spartanDisplay) {
            objectInstance.currentStepObject.parent().css({
              marginLeft: '-' + (objectInstance.currentStepObject.width() * objectInstance.currentStepObject.index()) + 'px'
            });

            objectInstance.formObject.data('uff').Libraries.EventManager.triggerEvent(
                objectInstance.onStepDisplayEvent,
                objectInstance.currentStepObject
            );
          } else {
            objectInstance.currentStepObject.parent().stop().animate({
              marginLeft: '-' + (objectInstance.currentStepObject.width() * objectInstance.currentStepObject.index()) + 'px'
            }, 500, function(){
              objectInstance.currentStepObject.find(':input:first').focus();

              objectInstance.formObject.data('uff').Libraries.EventManager.triggerEvent(
                  objectInstance.onStepDisplayEvent,
                  objectInstance.currentStepObject
              );
            });
          }
        }
      },

      Init : function(formObject) {
        this.formObject   = formObject;
        this.stepObjects  = formObject.find(this.stepIdentifier);
        this.stepCount    = this.stepObjects.length;

        this._assignStepNumbersToObjects();

        this._serveStepObject(this.stepObjects.filter(':first'), true);
        this._displayStepHandler();
        this._previousStepHandler();
        this._nextStepHandler();
        this._finishStepHandler();
      },

      _assignStepNumbersToObjects : function() {
        var objectInstance = this;

        this.stepObjects.each(function(){
          jQuery(this).attr('data-step-number', jQuery(this).index())
        });
      },

      _serveStepObject : function(stepObject, spartanDisplay) {
        var objectInstance = this;

        spartanDisplay = typeof spartanDisplay == "undefined" ? false : spartanDisplay;

        if(this.currentStep != stepObject.index()) {
          if(typeof this.currentStepObject.attr !== "undefined" &&
              typeof this.currentStepObject.attr('data-step-after') !== "undefined") {
            eval(this.currentStepObject.attr('data-step-after'));
          }

          if(!jQuery.isEmptyObject(this.currentStepObject))
            if(this.currentStep < stepObject.index())
              if(!objectInstance.formObject.data('uff').Helpers.Validation.IsValidSection(this.currentStepObject))
                return;
        }

        this.currentStep       = stepObject.index();
        this.currentStepObject = stepObject;

        this.formObject.data('uff').Helpers.Validation.hideErrors(this.currentStepObject);

        this._serverStepObjectWidgetsHandler();

        var displayMethodAlias =
            (typeof this.formObject.attr(this.stepDisplayMethodAttribute) !== "undefined"
                ? this.formObject.attr(this.stepDisplayMethodAttribute) : this.defaultStepDisplayMethod);

        this.stepDisplayMethods[displayMethodAlias](objectInstance, spartanDisplay);
      },

      _serverStepObjectWidgetsHandler : function() {
        if(this.currentStep == (this.stepCount - 1)) {
          this.formObject.find(this.stepNextIdentifier).hide();
          this.formObject.find(this.stepFinishIdentifier).show();
        } else {
          this.formObject.find(this.stepNextIdentifier).show();
          this.formObject.find(this.stepFinishIdentifier).not(this.stepForceDisplayIdentifier).hide();
        }
      },

      _displayStepHandler  : function() {
        var objectInstance = this;

        this.formObject.on('click', this.stepDisplayIdentifier, function(event){
          if(typeof jQuery(this).attr('href') != "undefined")
            event.preventDefault();

          objectInstance._serveStepObject(
              objectInstance.stepObjects.eq(jQuery(this).attr(objectInstance.stepDisplayNumber))
          );
        });
      },

      _previousStepHandler : function() {
        var objectInstance = this;

        this.formObject.on('click', this.stepPreviousIdentifier, function(event){
          if(typeof jQuery(this).attr('href') != "undefined")
            event.preventDefault();

          objectInstance._serveStepObject(objectInstance.currentStepObject.prev());
        });
      },

      _nextStepHandler : function() {
        var objectInstance = this;

        this.formObject.on('click', this.stepNextIdentifier, function(event){
          if(typeof jQuery(this).attr('href') != "undefined")
            event.preventDefault();

          objectInstance._serveStepObject(objectInstance.currentStepObject.next());
        });
      },

      _finishStepHandler : function() {
        var objectInstance = this;

        this.formObject.on('click', this.stepFinishIdentifier, function(event){
          event.preventDefault();

          if(!jQuery.isEmptyObject(objectInstance.currentStepObject))
            if(!objectInstance.formObject.data('uff').Helpers.Validation.IsValidSection(objectInstance.currentStepObject))
              return;

          objectInstance.formObject.submit();
        });
      }
    },

    InputMirror : {

      formObject          : {},
      inputIdentifier     : '[data-input-mirror]',
      attrInputIdentifier : 'data-input-mirror',
      inputEventListeners : 'keyup change',

      Init : function(formObject) {
        var objectInstance = this;

        this.formObject = formObject;

        this.formObject.find(this.inputIdentifier).each(function() {
          var mirrorObject = jQuery(this),
              inputObject  = objectInstance.formObject.find(jQuery(this).attr(objectInstance.attrInputIdentifier));

          mirrorObject.html(objectInstance._fetchMirrorObjectHTML(inputObject));

          inputObject.bind(objectInstance.inputEventListeners, function(){
            mirrorObject.html(objectInstance._fetchMirrorObjectHTML(jQuery(this)));
          });
        });
      },

      _fetchMirrorObjectHTML : function(inputObject) {
        if(inputObject.length == 0)
          return false;

        if (inputObject[0].nodeName == 'SELECT')
          return inputObject.find('> option[value="' + inputObject.val() + '"]').html();

        if(inputObject[0].nodeName == 'INPUT' && inputObject.attr('type') == 'checkbox') {
          if(inputObject.is(':checked'))
            return (typeof inputObject.attr('id') !== "undefined" ? jQuery('label[for=' + inputObject.attr('id') + ']').text() : 'Yes');
          else
            return '';
        }

        if(typeof inputObject.attr('type') !== "undefined")
          if(inputObject.attr('type') == 'password') {
            var ret = '';

            for(var i = 0; i < inputObject.val().length; ret += '*', i++);

            return ret;
          }

        return inputObject.val();
      }

    },

    HorizontalSelect : {

      Init : function(formObject) {
        formObject.find('select.eh-select').each(function(){
          var instance = jQuery.extend(true, {}, formObject.data('uff').Libraries.factory(
              formObject.data('uff').Libraries.jQueryEHSelect)
          );

          instance.Init(jQuery(this));
        });
      }

    },

    EMSelect : {

      Init : function(formObject) {
        formObject.find('select.em-select').each(function(){
          var instance = jQuery.extend(true, {}, formObject.data('uff').Libraries.factory(
              formObject.data('uff').Libraries.jQueryEMSelect)
          );

          instance.Init(jQuery(this));
        });
      }

    },

    MTSelect : {

      identifier     : '[data-mt]',
      settingsPrefix : 'data-mt-',

      Init : function(formObject) {
        var objectInstance = this;

        formObject.find(this.identifier).each(function(mtSelectInstanceNumber){
          if(jQuery(this).hasClass('dispatched'))
            return;

          jQuery(this).addClass('dispatched');

          var instance = jQuery.extend(true, {}, formObject.data('uff').Libraries.factory(
              formObject.data('uff').Libraries.jQueryMTSelect)
          );

          var attributes = {}, params = {};

          jQuery.each( jQuery(this)[0].attributes, function( index, attr ) {
            attributes[ attr.name ] = attr.value;
          } );

          jQuery.each(attributes, function(key, value){
            if(key.indexOf(objectInstance.settingsPrefix) == 0) {
              var name = key.replace(objectInstance.settingsPrefix, '');

              name = name.replace(/-/g, '_');

              params[name] = value;
            }
          });

          params.namespace = "mt_select_" + mtSelectInstanceNumber;

          instance.Init(jQuery(this), params);
        });
      }

    },

    LengthController : {

      formObject              : {},
      maxLengthIdentifier     : "[data-length-max]",
      attrMaxLengthIdentifier : "data-length-max",

      Init : function(formObject) {
        this.formObject = formObject;

        this.setMaxLength();
      },

      setMaxLength : function() {
        var objectInstance = this;

        this.formObject.on('keyup keydown', this.maxLengthIdentifier, function() {
          if(jQuery(this).val().length > jQuery(this).attr(objectInstance.attrMaxLengthIdentifier)) {
            jQuery(this).val(
                jQuery(this).val().slice(jQuery(this).val().length -
                    jQuery(this).attr(objectInstance.attrMaxLengthIdentifier),
                    jQuery(this).val().length));
            jQuery(this).trigger('change');
          }
        });
      }

    },

    DigitController : {

      formObject              : {},
      digitOnlyIdentifier     : "[data-digits-only]",

      Init : function(formObject) {
        this.formObject = formObject;

        this.setDigitOnly();
      },

      setDigitOnly : function() {
        this.formObject.on('keyup keydown', this.digitOnlyIdentifier, function() {
          var cleanValue = parseFloat(jQuery(this).val());

          jQuery(this).val((isNaN(cleanValue) ? '' : cleanValue));

          jQuery(this).trigger('change');
        });
      }

    },

    ElegantSoundController : {

      identifier         : "[data-elegant-sound]",
      soundPathAttribute : "data-elegant-sound",
      eventAttribute     : 'data-elegant-sound-on',
      durationAttribute  : 'data-elegant-sound-duration',
      formObject : {},
      defaultEvent       : "click change keyup",
      defaultDuration    : "1000",

      Init : function(formObject) {
        this.formObject = formObject;

        this.bindSound();
      },

      bindSound : function() {
        var objectInstance = this;

        this.formObject.find(this.identifier).each(function(){
          var events = (typeof jQuery(this).attr(objectInstance.eventAttribute) === "undefined" ? objectInstance.defaultEvent : jQuery(this).attr(objectInstance.eventAttribute));

          jQuery(this).on(events, function(){
            if(jQuery(this).css('opacity') == 0
                || jQuery(this).parent().css('opacity') == 0)
              return;

            var sound = '<audio id="elegant_sound_effect" controls="controls" autoplay="autoplay" style="display: none;">' +
                '<source src="' + jQuery(this).attr(objectInstance.soundPathAttribute) + '" type="audio/mpeg" />' +
                'Your browser does not support the audio element.' +
                '</audio>';

            $('body').append(sound);

            setTimeout(function(){
              $("#elegant_sound_effect").remove()
            }, (typeof jQuery(this).attr(objectInstance.durationAttribute) === "undefined" ? objectInstance.defaultDuration : jQuery(this).attr(objectInstance.durationAttribute)));
          });
        })
      }

    },

    InputReplicate : {

      inputIdentifier           : '[data-input-replica]',
      attrInputIdentifier       : 'data-input-replica',
      inputOnIdentifier         : '[data-input-replica-on]',
      attrInputOnIdentifier     : 'data-input-replica-on',
      inputOnRuleIdentifier     : '[data-input-replica-on-rule]',
      attrInputOnRuleIdentifier : 'data-input-replica-on-rule',

      defaultOnRule             : ':checked',

      namespace  : 'input-replicate',
      formObject : {},

      Init : function(formObject) {
        var objectInstance = this;

        this.formObject = formObject;

        this.formObject.find(this.inputIdentifier).each(function(){
          if(typeof jQuery(this).attr(objectInstance.attrInputOnIdentifier) === "undefined") {
            objectInstance._assignInputReplica(jQuery(this));
          } else {
            objectInstance._assignInputReplicaWithOnHandler(jQuery(this));
          }
        });
      },

      _assignInputReplicaWithOnHandler : function(inputObject) {
        var objectInstance   = this,
            onEventHandler   = this.formObject.find(inputObject.attr(this.attrInputOnIdentifier)),
            onEventRule      = (typeof inputObject.attr(this.attrInputOnRuleIdentifier) == "undefined" ?
                this.defaultOnRule : inputObject.attr(this.attrInputOnRuleIdentifier));

        if(onEventHandler.length > 0) {
          this._evaluateInputReplica(inputObject, onEventHandler, onEventRule);

          onEventHandler.bind('change click', function(){
            objectInstance._evaluateInputReplica(inputObject, onEventHandler, onEventRule);
          });
        }
      },

      _evaluateInputReplica : function(inputObject, onEventHandler, onEventRule) {
        if(onEventHandler.is(onEventRule)) {
          this._assignInputReplica(inputObject);
        } else {
          this._unAssignInputReplica(inputObject);
        }
      },

      _assignInputReplica : function(inputObject) {
        var replicaInput = this.formObject.find(inputObject.attr(this.attrInputIdentifier));

        inputObject.attr('disabled', 'disabled');
        inputObject.val(replicaInput.val());
        inputObject.trigger('change');

        replicaInput.bind('change.' + this.namespace + ' keyup.' + this.namespace, function(){
          inputObject.val(replicaInput.val());
          inputObject.trigger('keyup');
          inputObject.trigger('change');
        });
      },

      _unAssignInputReplica : function(inputObject) {
        var replicaInput = this.formObject.find(inputObject.attr(this.attrInputIdentifier));

        inputObject.removeAttr('disabled');
        replicaInput.unbind('change.' + this.namespace + ' keyup.' + this.namespace);
      }

    }

  },

  Libraries : {

    factory : function(libraryObject) {
      var instance = jQuery.extend(true, {}, libraryObject);

      return instance;
    },

    EventManager : {

      eventList : {},

      init : function() {

      },

      registerEvent : function(event_identifier) {
        this.eventList[event_identifier] = [];
      },

      unRegisterEvent : function(event_identifier) {
        if(typeof this.eventList[event_identifier] != "undefined")
          delete this.eventList[event_identifier];
      },

      triggerEvent  : function(event_identifier, data) {
        data = typeof data != "undefined" ? data : {};

        if(typeof this.eventList[event_identifier] != "undefined") {
          var currentEventInformation = this.eventList[event_identifier];

          for(var currentListenerIndex in currentEventInformation) {
            var currentListener       = currentEventInformation[currentListenerIndex],
                currentListenerMethod = currentListener['method'];

            currentListener.object[currentListenerMethod].call(currentListener.object, data);
          }
        }
      },

      listenEvent : function(event_identifier, object, method) {
        if(typeof this.eventList[event_identifier] == "undefined")
          this.registerEvent(event_identifier);

        this.eventList[event_identifier][this.eventList[event_identifier].length] = {
          'object' : object,
          'method' : method
        }
      }
    },

    StoryBox : {

      containerObject             : {},
      containerPanels             : [],
      settings : {
        displayEffect         : ['bounceInUp', 'bounceInRight', 'bounceInDown', 'bounceInLeft'],
        hideEffect            : ['bounceOutUp', 'bounceOutRight', 'bounceOutDown', 'bounceOutLeft']
      },

      namespace                   : 'storybox',

      Init : function(container, settings) {
        settings = typeof(settings) == "undefined" ? {} : settings;

        this.namespace = this.namespace + '-' +  parseInt(Math.random() * 1000);

        this.containerObject = container;
        this.settings = jQuery.extend(1, this.settings, settings);

        this.setContainerPanels();
        this.loadPanels();
      },

      setContainerPanels : function() {
        var objectInstance = this;

        this.containerObject.find('[data-sb]').each(function(){
          objectInstance.containerPanels[objectInstance.containerPanels.length]
              = {
            'container'       : jQuery(this),
            'displayEffect'   :
                (jQuery.trim(jQuery(this).attr('data-sb')) == '' ?
                    objectInstance.settings.displayEffect :
                    (
                        jQuery(this).attr('data-sb').indexOf(',') != -1
                            ? jQuery(this).attr('data-sb').split(',')
                            : jQuery(this).attr('data-sb')
                        )
                    ),
            'hideEffect'      :
                typeof jQuery(this).attr('data-sb-hide') != "undefined" ?
                    (
                        jQuery.trim(jQuery(this).attr('data-sb-hide')) == '' ?
                            objectInstance.settings.hideEffect :
                            (
                                jQuery(this).attr('data-sb-hide').indexOf(',') != -1
                                    ? jQuery(this).attr('data-sb-hide').split(',')
                                    : jQuery(this).attr('data-sb-hide')
                                )
                        ) : false
          };
          $(this).removeClass('sb-effect-displayed').css('opacity', 0);
        });
      },

      loadPanels   : function() {
        var objectInstance = this;

        jQuery(window).bind('scroll.' + this.namespace, function(){
          objectInstance._handleDisplay();
        });

        objectInstance._handleDisplay();
      },

      _handleDisplay : function() {
        var currentScrollTop        = jQuery(window).scrollTop() + jQuery(window).height(),
            objectInstance          = this;
        jQuery.each(objectInstance.containerPanels, function(key, panel){
          if(panel.container.hasClass('sb-effect-running'))
            return;

          if(currentScrollTop >= panel.container.offset().top
              && currentScrollTop <= ( panel.container.offset().top + (panel.container.height() * 0.7) + jQuery(window).height())) {
            if(panel.container.hasClass('sb-effect-displayed'))
              return;

            panel.container.addClass('sb-effect-displayed sb-effect-running');

            var effect = objectInstance._getRandomSettingElement(panel.displayEffect);

            if(typeof panel.container.data('sb-effect') != "undefined")
              panel.container.removeClass('animated ' + panel.container.data('sb-effect'));

            panel.container.css('opacity', 1);
            panel.container.addClass('animated ' + effect);
            panel.container.data('sb-effect', effect);

            setTimeout(function(){
              panel.container.removeClass('sb-effect-running');
            }, 1000);
          } else if(panel.container.hasClass('sb-effect-displayed')) {
            if(panel.hideEffect != false) {
              panel.container.removeClass('sb-effect-displayed animated ' + panel.container.data('sb-effect'));

              var effect = objectInstance._getRandomSettingElement(panel.hideEffect);

              panel.container.addClass('animated ' + effect);
              panel.container.data('sb-effect', effect);
              panel.container.css('opacity', 1);
            }
          }
        });
      },

      unLoadPanels : function() {
        var objectInstance = this;

        this.containerPanels.each(function(){
          jQuery(this).unbind(objectInstance.namespace);
        });
      },


      _getRandomSettingElement : function(element) {
        return (element instanceof Array ? element[ Math.floor( Math.random() * element.length )] : element);
      }

    },

    StoryTale : {

      containerObject             : {},
      containerPanels             : [],
      namespace                   : 'storytale',
      displayEffect               : ['rotateIn', 'rotateInDownLeft', 'rotateInDownRight', 'rotateInUpLeft', 'rotateInUpRight'],

      Init : function(container, displayEffect) {
        this.namespace = this.namespace + '-' +  parseInt(Math.random() * 1000);

        this.containerObject = container;
        this.displayEffect   = displayEffect;

        this.setContainerPanels();
        this.loadPanels();
      },

      setContainerPanels : function() {
        var objectInstance = this;

        this.containerPanels = this.containerObject.find('[data-st-item], [data-st-container]');

        this.containerPanels.each(function(){
          if(typeof jQuery(this).attr('data-st-item') != "undefined") {
            var html    = jQuery(this).html(),
                newHTML = '';

            for(var i = 0; i < html.length; i++)
              newHTML += '<span style="opacity: 0.0;display: inline-block;">' + (html[i] == " " ? '&nbsp;' : html[i]) + '</span>';

            jQuery(this).html(newHTML);
          } else {
            jQuery(this).css('opacity', '0.0');
          }
        });
      },

      loadPanels   : function() {
        var objectInstance = this;

        jQuery(window).bind('scroll.' + this.namespace, function(){
          objectInstance._handleDisplay();
        });

        objectInstance._handleDisplay();
      },

      _handleDisplay : function() {
        var currentScrollTop        = jQuery(window).scrollTop() + jQuery(window).height(),
            objectInstance          = this,
            panel                   = this.containerPanels.not('.story-tale-served').first();

        if(panel.length == 0) {
          jQuery(window).unbind('scroll.' + this.namespace);
          return;
        }

        if( !panel.hasClass('story-tale-served')
            && !panel.hasClass('story-tale-serving')
            && currentScrollTop >= panel.offset().top
            && currentScrollTop <= ( panel.offset().top + (panel.height() * 0.7) + jQuery(window).height())) {

          if(typeof panel.attr('data-st-item') == "undefined") {
            panel.addClass('story-tale-serving');

            var effect = panel.attr('data-st-container') == '' ? objectInstance.displayEffect : panel.attr('data-st-container').split(',');

            panel.css('opacity', '1').addClass('animated ' + objectInstance._getRandomEffect(effect));

            setTimeout(function(){
              panel.removeClass('story-tale-serving').addClass('story-tale-served');
              objectInstance._handleDisplay();
            }, 1000);

            return;
          }

          if(!(panel.find('> span').not('.story-tale-served').length > 0)) {
            panel.addClass('story-tale-served');
            return;
          }

          panel.addClass('story-tale-serving');

          panel.find('> span').each(function(){

            var effect = panel.attr('data-st-item') == '' ? objectInstance.displayEffect : panel.attr('data-st-item').split(',');

            jQuery(this).css('opacity', 1).addClass('story-tale-served animated ' + objectInstance._getRandomEffect(effect));
          }).promise().done(function() {
                setTimeout(function(){
                  panel.removeClass('story-tale-serving').addClass('story-tale-served');
                  objectInstance._handleDisplay();
                }, 1000);
              });

        }
      },

      _getRandomEffect : function(effect) {
        return (effect instanceof Array ? effect[ Math.floor( Math.random() * effect.length )] : effect);
      }

    },

    jQueryEHSelect : {

      selectObject        : {},
      selectOptionsList   : {}, // value => text
      selectedOption      : '', // value

      Init : function(selectObject) {
        if(this.MobileHelper.any() || selectObject.hasClass('dispatched'))
          return;

        selectObject.addClass('dispatched');

        this.selectObject = selectObject;
        this.selectObject.data('eh_select', this);

        this.AttributeBasedSettings.Init(this);

        this.switchToInteractiveMode();
      },

      switchToInteractiveMode : function() {
        this.Helper.Init(this);
        this.InteractiveSection.Init(this);
      },

      InteractiveSection : {

        containerObject         : {},
        containerClass          : 'jquery-eh-interactive-container',

        Controller : {},

        Init : function(Controller) {
          this.Controller = Controller;

          this.Controller.selectObject.hide();
          this.SetContainer();
          this.SelectController.Init(this);
          this.VisualController.Init(this);
        },

        SetContainer : function() {
          this.Controller.selectObject.after(this._getContainerHTML());
          this.containerObject = this.Controller.selectObject.next();
        },

        _getContainerHTML : function() {
          var objectInstance = this;

          var html = "";

          html += '<div class="' + objectInstance.containerClass + '">';

          html += '</div>';

          return html;
        },

        SelectController : {

          selectObject            : {},
          selectClass             : 'jquery-eh-interactive-select',
          selectOptionDisplayKey  : 'jquery-eh-select-placeholder',
          selectOptionDisplayText : 'Please Select...',

          InteractiveSectionController : {},

          Init : function(InteractiveSectionController) {
            this.InteractiveSectionController = InteractiveSectionController;
            this.setSelect();
            this.setSelectSync();
          },

          setSelect : function() {
            this.InteractiveSectionController.containerObject.append(this.getSelectHTML());
            this.selectObject = this.InteractiveSectionController.containerObject.find('> .' + this.selectClass);
          },

          resetSelect : function() {
            this.selectObject.replaceWith(this.selectObject.clone());

            this.selectObject = this.InteractiveSectionController.containerObject.find('> .' + this.selectClass);

            this.setSelectSync();
          },

          getSelectHTML : function() {
            var objectInstance = this,
                html = '';

            html +=   '<select class="' + this.selectClass + '">';
            html +=     '<option value="' + this.selectOptionDisplayKey + '">' + this.selectOptionDisplayText + '</option>';
            jQuery.each(objectInstance.InteractiveSectionController.Controller.selectOptionsList, function(key, value) {
              html     += '<option value="' + key + '" ' + (objectInstance.InteractiveSectionController.Controller.selectedOption == key ? 'selected="selected"' : '') + '>' + value + '</option>';
            });
            html +=   '</select>';

            return html;
          },

          setSelectSync : function() {
            var objectInstance = this;

            this.selectObject.unbind('change').bind('change', function() {
              if(jQuery(this).val() != objectInstance.selectOptionDisplayKey) {
                objectInstance
                    .InteractiveSectionController
                    .Controller
                    .selectObject
                    .val(jQuery(this).val()).trigger('change');
                objectInstance.InteractiveSectionController.Controller
                    .selectObject
                    .find('> option[value="' + jQuery(this).val() + '"]')
                    .attr('selected', 'selected');
              }

              if(typeof objectInstance.InteractiveSectionController.VisualController.informationListObject != "undefined") {
                objectInstance.InteractiveSectionController.VisualController.informationListObject.find('> li').removeClass('selected');
                objectInstance.InteractiveSectionController.VisualController.informationListObject.find('> li[data-key="' + jQuery(this).val() + '"]').addClass('selected');
              }
            });
          }

        },

        VisualController : {

          InteractiveSectionController : {},

          informationListObject   : {},
          informationListClass    : 'jquery-eh-interactive-information-list',

          Init : function(InteractiveSectionController) {
            this.InteractiveSectionController = InteractiveSectionController;
            this.setVisual();
            this.ArrangeInformationList();
          },

          setVisual : function() {
            this.InteractiveSectionController.containerObject.append(this.getVisualHTML());
            this.informationListObject = this.InteractiveSectionController.containerObject.find('> .' + this.informationListClass);
          },

          getVisualHTML : function() {
            var objectInstance = this,
                html = '';

            html +=   '<ul class="' + this.informationListClass + '" style="display:none">';

            jQuery.each(this.InteractiveSectionController.Controller.selectOptionsList, function(key, value){
              html += '<li data-key="' + key + '" ' + (objectInstance.InteractiveSectionController.Controller.selectedOption == key ? 'class="selected"' : '') + '>' + value + '</li>';
            });

            html +=   '</ul>';

            return html;
          },

          SetInteraction : function() {
            var objectInstance = this;

            this.InteractiveSectionController.SelectController.selectObject.unbind('focus').bind('focus', function(event) {
              event.stopImmediatePropagation();
              event.preventDefault();

              jQuery(this).blur();
              objectInstance.InteractiveSectionController.SelectController.resetSelect();
              objectInstance.SetInteraction();

              if(objectInstance.informationListObject.is(':hidden')) {
                objectInstance._openInteraction(true);
              } else {
                objectInstance._closeInteraction();
              }
            });

            this.informationListObject.find('> li').unbind('click').bind('click', function(){
              objectInstance.InteractiveSectionController
                  .SelectController
                  .selectObject
                  .val(jQuery(this).attr('data-key'))
                  .trigger('change');
              objectInstance._closeInteraction();
            });
          },

          DisableInteraction : function() {
            this.InteractiveSectionController.SelectController.selectObject.unbind('focus');
            this.informationListObject.find('> li').unbind('click');
          },

          _openInteraction  : function(clickEvent) {
            this.informationListObject.show();

            var response = this.ArrangeInformationList(clickEvent);

            if(response == false) {
              return;
            }

            var width = this.informationListObject.width();

            this.informationListObject.css('width', 0);

            this.informationListObject.animate({
              width : width
            });

            this.informationListObject.addClass('open');
          },

          _closeInteraction : function() {
            if(this.informationListObject.hasClass('open')) {
              this.informationListObject.animate({
                width : 0
              }, function(){
                jQuery(this).hide();
                jQuery(this).removeClass('open');
              });
            } else {
              this.informationListObject.hide();
            }
          },

          ArrangeInformationList : function() {
            var selectObject = this.InteractiveSectionController.SelectController.selectObject,
                theListWidth = this._arrangeCalculateListWidth(),
                maxListWidth = this._arrangeCalculateMaxListWidth();

            if(theListWidth > maxListWidth) {
              this._closeInteraction();
              this.DisableInteraction();

              return false;
            } else {
              this.SetInteraction();
            }

            this._arrangePositioningHandler();

            this.informationListObject.css('height', (
                parseInt(selectObject.height(), 10)
                    + parseInt(selectObject.css('padding-top'), 10)
                    + parseInt(selectObject.css('padding-bottom'), 10)
                    + parseInt(selectObject.css('margin-top'), 10)
                    + parseInt(selectObject.css('margin-bottom'), 10)
                ));

            this.informationListObject.css('width', (theListWidth > maxListWidth ? maxListWidth : theListWidth));

            return true;
          },

          _arrangePositioningHandler : function() {
            var selectObject = this.InteractiveSectionController.SelectController.selectObject;

            this.informationListObject.css('position', 'absolute');
            this.informationListObject.css('left', (
                parseInt(selectObject.width(), 10)
                    + parseInt(selectObject.css('padding-left'), 10)
                    + parseInt(selectObject.css('padding-right'), 10)
                    + parseInt(selectObject.css('margin-left'), 10)
                    + parseInt(selectObject.css('margin-right'), 10)
                ));
            this.informationListObject.css('top', selectObject.position().top);
          },

          _arrangeCalculateListWidth : function() {
            var theListWidth = 0;

            this.informationListObject.css('width', '100%');

            jQuery(this.informationListObject.find('> li')).each(function(){
              theListWidth += (
                  parseInt(jQuery(this).width(), 10)
                      + parseInt(jQuery(this).css('padding-left'), 10)
                      + parseInt(jQuery(this).css('padding-right'), 10)
                      + parseInt(jQuery(this).css('margin-left'), 10)
                      + parseInt(jQuery(this).css('margin-right'), 10)
                  );
            });

            theListWidth += (
                parseInt(this.informationListObject.css('padding-left'), 10)
                    + parseInt(this.informationListObject.css('padding-right'), 10)
                    + parseInt(this.informationListObject.css('margin-left'), 10)
                    + parseInt(this.informationListObject.css('margin-right'), 10)
                );

            // 3px error

            theListWidth += (this.informationListObject.find('> li').length * 3);

            return theListWidth;
          },

          _arrangeCalculateMaxListWidth : function() {
            return parseInt(jQuery('html').width(), 10) - parseInt(this.informationListObject.offset().left, 10);
          },

          ArrangeInformationListBasedOnEvents : function() {
            var objectInstance = this;

            jQuery(window).bind('resize onscreenrotate', function(){
              objectInstance.ArrangeInformationList();
            });
          }

        }

      },

      AttributeBasedSettings : {

        Controller : {},

        Init : function(Controller) {
          this.Controller = Controller;

          this.handleSettings();
        },

        handleSettings : function() {
          var objectInstance = this,
              params         = this.getParams();

          jQuery.each(params, function(key, value){
            objectInstance._recursiveSettingAssign(objectInstance.Controller, key, value);
          });
        },

        getParams : function() {
          var objectInstance = this, params = {};

          jQuery.each( objectInstance.Controller.selectObject[0].attributes, function( index, attr ) {
            var attrKey   = attr.name,
                attrValue = attr.value;

            if(attrKey.indexOf('data-eh-') == 0) {
              attrKey = attrKey.replace('data-eh-', '');

              var builtAttribute = '';

              jQuery.each(attrKey.split('-'), function(keyIndex, key){
                var context = key.replace(/_/gi, ' ');
                context = objectInstance.Controller.Helper.ucWords(context);
                context = context.replace(/ /gi, '');

                if(key[0] != '_')
                  context = (context.substr(0, 1)).toLowerCase() + context.substr(1);

                builtAttribute = (builtAttribute == '' ? context : builtAttribute + '-' + context);
              });

              params[builtAttribute] = attrValue;
            }
          });

          return params;
        },

        _recursiveSettingAssign : function(object, key, value) {
          if(key.indexOf('-') !== -1) {
            this._recursiveSettingAssign(object[key.substr(0, key.indexOf('-'))],
                key.substr(key.indexOf('-') + 1),
                value);
          } else {
            object[key] = value;
          }
        }

      },

      Helper : {

        Controller : {},

        Init : function(Controller) {
          this.Controller = Controller;

          this.setCurrentSelectElements();
        },

        setCurrentSelectElements : function() {
          var objectInstance = this;

          objectInstance.Controller.selectOptionsList = this.getCurrentSelectElements();
          objectInstance.Controller.selectedOption    = this.getCurrentSelectedElement();
        },

        getCurrentSelectElements : function() {
          var ret = {};

          this.Controller.selectObject.find('> option').each(function(){
            ret[jQuery(this).attr('value')] = jQuery(this).html();
          });

          return ret;
        },

        getCurrentSelectedElement : function() {
          return this.Controller.selectObject.find('> option[selected="selected"]').val();
        },

        ucWords : function(str) {
          return (str + '').replace(/^([a-z\u00E0-\u00FC])|\s+([a-z\u00E0-\u00FC])/g, function ($1) {
            return $1.toUpperCase();
          });
        }

      },

      MobileHelper : {
        Android: function() {
          return navigator.userAgent.match(/Android/i);
        },
        BlackBerry: function() {
          return navigator.userAgent.match(/BlackBerry/i);
        },
        iOS: function() {
          return navigator.userAgent.match(/iPhone|iPad|iPod/i);
        },
        Opera: function() {
          return navigator.userAgent.match(/Opera Mini/i);
        },
        Windows: function() {
          return navigator.userAgent.match(/IEMobile/i);
        },
        any: function() {
          return (this.Android() || this.BlackBerry() || this.iOS() || this.Opera() || this.Windows());
        }
      }

    },

    jQueryEMSelect : {

      selectObject        : {},
      selectOptionsList   : {}, // value => text
      selectedOptionsList : [], // value

      Init : function(selectObject) {
        if(selectObject.hasClass('dispatched'))
          return;

        selectObject.addClass('dispatched');

        this.selectObject = selectObject;
        this.selectObject.data('em_select', this);

        this.switchToInteractiveMode();
      },

      switchToInteractiveMode : function() {
        this.Helper.Init(this);
        this.InteractiveSection.Init(this);
        this.DOMObserver.Init(this);
      },

      InteractiveSection : {

        containerObject         : {},
        selectObject            : {},
        informationListObject   : {},
        containerClass          : 'jquery-em-interactive-container',
        selectClass             : 'jquery-em-interactive-select',
        informationListClass    : 'jquery-em-interactive-information-list',
        selectOptionDisplayKey  : 'jquery-em-select-placeholder',
        selectOptionDisplayText : 'Please Select...',

        Controller : {},

        Init : function(Controller) {
          this.Controller = Controller;

          this.Controller.selectObject.hide();
          this.SetContainer();
          this.SetSelectInteraction();
        },

        Reset : function() {
          this.containerObject.remove();
          this.SetContainer();
          this.SetSelectInteraction();
        },

        SetContainer : function() {
          this.Controller.selectObject.after(this._getContainerHTML());

          this.containerObject = this.Controller.selectObject.next();

          this.selectObject    = this.containerObject.find('> .' + this.selectClass );
          this.informationListObject = this.containerObject.find('> .' + this.informationListClass);

          this.SetSelectRemove();
        },

        _getContainerHTML : function() {
          var objectInstance = this;

          var html = "";

          html += '<div class="' + objectInstance.containerClass + '">';
          html +=   '<select class="' + this.selectClass + '">';

          html +=     '<option value="' + this.selectOptionDisplayKey + '">' + this.selectOptionDisplayText + '</option>';

          jQuery.each(objectInstance.Controller.selectOptionsList, function(key, value){
            html += '<option value="' + key + '" ' + (jQuery.inArray(key, objectInstance.Controller.selectedOptionsList) != -1 ? 'disabled="disabled"' : '') + '>' + value + '</option>';
          });

          html +=   '</select>';

          html +=   '<ul class="' + objectInstance.informationListClass + '">';

          jQuery.each(objectInstance.Controller.selectedOptionsList, function(index, key){
            html += objectInstance._getSelectLineItem(key, objectInstance.Controller.selectOptionsList[key]);
          });

          html +=   '</ul>';

          html += '</div>';

          return html;
        },

        SetSelectInteraction : function() {
          var objectInstance = this;

          this.selectObject.bind('change', function(){
            if(jQuery(this).val() == objectInstance.selectOptionDisplayKey)
              return;

            var key   = jQuery(this).val(),
                value = objectInstance.selectObject.find('> option[value="' + key + '"]').html();

            objectInstance.Controller.selectObject.val(
                jQuery.merge(
                    (
                        jQuery.isEmptyObject(objectInstance.Controller.selectObject.val())
                            ? [] : objectInstance.Controller.selectObject.val()
                        ),
                    [key]
                )
            );

            objectInstance.selectObject.find('> option[value="' + key + '"]').attr('disabled', 'disabled');
            objectInstance.selectObject.val(objectInstance.selectOptionDisplayKey);

            objectInstance.informationListObject.append(objectInstance._getSelectLineItem(key, value));
            objectInstance.informationListObject.find(' > *:last').hide().slideDown('slow');
            objectInstance.SetSelectRemove();
          });
        },

        SetSelectRemove : function() {
          var objectInstance = this;

          this.informationListObject.find('> li > span.remove').not('.binded').addClass('binded').bind('click touchstart', function(){
            var key                    = jQuery(this).parent('li').attr('data-option-key'),
                controllerSelectValues = objectInstance.Controller.selectObject.val();

            jQuery.each(controllerSelectValues, function(selectVKey, selectVValue){
              if(selectVValue == key)
                delete controllerSelectValues[selectVKey];
            });

            objectInstance.Controller.selectObject.val(controllerSelectValues);
            objectInstance.selectObject.find('> option[value="' + key + '"]').removeAttr('disabled');

            jQuery(this).parent('li').slideUp('slow', function(){
              jQuery(this).remove();
            });
          });
        },

        _getSelectLineItem : function(key, value) {
          return '<li data-option-key="' + key + '"><span class="text">' + value + '</span><span class="remove">Remove</span><span class="clear"></span></li>';
        }

      },

      DOMObserver : {

        Observer   : {},
        Controller : {},

        Init : function(Controller) {
          var objectInstance = this;

          this.Controller = Controller;

          if(typeof MutationObserver == "function") {
            this.Observer = new MutationObserver(function(mutations) {
              objectInstance._observed();
            });

            this.Observer.observe(objectInstance.Controller.selectObject[0], { childList: true });
          }
        },

        _observed : function() {
          var aKeys = Object.keys(this.Controller.Helper.getCurrentSelectElements()).sort();
          var bKeys = Object.keys(this.Controller.selectOptionsList).sort();

          if(!(JSON.stringify(aKeys) === JSON.stringify(bKeys))) {
            this.Controller.Helper.setCurrentSelectElements();
            this.Controller.InteractiveSection.Reset();
          }
        }

      },

      Helper : {

        Controller : {},

        Init : function(Controller) {
          this.Controller = Controller;

          this.setCurrentSelectElements();
        },

        setCurrentSelectElements : function() {
          var objectInstance = this;

          objectInstance.Controller.selectOptionsList   = this.getCurrentSelectElements();
          objectInstance.Controller.selectedOptionsList = this.getCurrentSelectedElements();
        },

        getCurrentSelectElements : function() {
          var objectInstance = this,
              ret = {};

          this.Controller.selectObject.find('> option').each(function(){
            ret[jQuery(this).attr('value')] = jQuery(this).html();
          });

          return ret;
        },

        getCurrentSelectedElements : function() {
          var objectInstance = this,
              ret = [];

          this.Controller.selectObject.find('> option:selected').each(function(){
            ret[ret.length] = jQuery(this).attr('value');
          });

          return ret;
        }

      }

    },

    jQueryMTSelect : {

      elementInformationMAP : {
        tagContainer : {
          'element' : 'span',
          'class'   : 'mt-tag-container'
        },
        tagElement  : {
          'element' : 'span',
          'class'   : 'mt-tag-element'
        },
        tagElementRemove : {
          'element' : 'a',
          'class'   : 'none',
          'content' : 'X'
        }
      },

      skeletonStructure : {
        'default' : {
          entryInformationListHTMLSkeleton : '<div class="mt_search_list_container">' +
              '{entries_information_list}' +
              '</div>',
          entryInformationSingleContainerIdentifier : '.mt_entry_container',
          entryInformationHTMLSkeleton :'<div class="mt_entry_container addTag" data-tag-id="{id}" data-tag-name="{name}">' +
              '<div class="left">' +
              '<img src="{picture_path}"/>' +
              '</div>' +
              '<div class="right">' +
              '<p class="name">{name|boldSearch}</p>' +
              '<p class="description">{description|boldSearch}</p>' +
              '</div>' +
              '</div>',
          responseMessageSkeleton : '<div class="mt_search_message">{message}</div>'
        }
      },

      entryInformationListHTMLSkeleton          : '',
      entryInformationSingleContainerIdentifier : '.',
      entryInformationHTMLSkeleton              : '',
      responseMessageSkeleton                   : '',

      triggerInformationMAP : {
        searchTriggerIdentifier     : ':input[data-mt-filter-control]',
        searchTriggerEvent          : 'keyup focus',
        searchTriggerMinimumLength  : 3,
        searchTriggerCSSSettings    : {
          'width' : 'auto'
        }
      },

      namespace           : 'mt_search',
      containerObject     : {},
      tagContainerObject  : {},
      requestURL          : '',
      requestMethod       : 'POST',
      requestSearchedTerm : 'mt_filter',
      requestSelectedTerms: 'mt_selected',
      requestExtraParams  : {},
      closeModalOnSelect  : 1,
      clearInputOnSelect  : 1,
      maxTags             : false,
      defaultValues       : [],
      tagInputType        : 'hidden',
      tagInputName        : 'tag',
      closeOnUnFocus      : 1,
      skeleton            : 'default',

      _currentAJAXRequestObject : false,

      Init : function(container, settings) {
        this._handleSettings(settings);
        this.containerObject = container;

        this.ModalHelper         = this.ModalHelper.Init(this);
        this.KeyNavigationHelper = this.KeyNavigationHelper.Init(this);

        this.prependTagContainer();
        this.assignFilterTriggers();
        this._handleDefaultValues();
        this.setWindowResizeEvent();
      },

      _handleSettings : function(settings) {
        var objectInstance = this;

        this.requestURL           = typeof settings.request_url      != "undefined"  ? settings.request_url     : '';
        this.requestMethod        = typeof settings.request_method   != "undefined"  ? settings.request_method  : this.requestMethod;
        this.requestSearchedTerm  = typeof settings.request_tag_name != "undefined"  ? settings.request_tag_name  : this.requestSearchedTerm;
        this.requestSelectedTerms = typeof settings.request_selected_tags_name != "undefined"
            ? settings.request_selected_tags_name  : this.requestSelectedTerms;

        this.closeModalOnSelect = typeof settings.close_on_select  != "undefined"  ? parseInt(settings.close_on_select)   : this.closeModalOnSelect;
        this.closeOnUnFocus     = typeof settings.close_on_unfocus != "undefined"  ? parseInt(settings.close_on_unfocus)  : this.closeOnUnFocus;
        this.clearInputOnSelect = typeof settings.clear_on_select  != "undefined"  ? parseInt(settings.clear_on_select)   : this.clearInputOnSelect;

        this.tagInputName       = typeof settings.tag_input_name   != "undefined"  ? settings.tag_input_name  : this.tagInputName;
        this.tagInputType       = typeof settings.tag_input_type   != "undefined"  ? settings.tag_input_type  : this.tagInputType;
        this.maxTags            = typeof settings.max_tags         != "undefined"  ? settings.max_tags        : this.maxTags;
        this.skeleton           = typeof settings.skeleton         != "undefined"  ? settings.skeleton        : this.skeleton;

        this.namespace          = typeof settings.namespace        != "undefined"  ? settings.namespace        : this.namespace;

        if(typeof settings.default_values != "undefined")
          this.defaultValues = typeof settings.default_values == "string" ? jQuery.parseJSON(settings.default_values) : settings.default_values;

        jQuery.each(settings, function(key, value){
          if(key.indexOf('custom_param_') == 0) {
            var name = key.replace('custom_param_', '');

            objectInstance.requestExtraParams[name] = value;
          }
        });

        jQuery.each(this.skeletonStructure[this.skeleton], function(key, value){
          objectInstance[key] = value;
        });
      },

      _handleDefaultValues : function() {
        if(this.defaultValues != false) {
          var objectInstance = this;

          jQuery.each(this.defaultValues, function(key, value){
            objectInstance.addTag(key, value);
          });
        }
      },

      prependTagContainer : function() {
        this.containerObject.prepend(
            '<'  + this.elementInformationMAP.tagContainer.element + ' ' +
                'class="' + this.elementInformationMAP.tagContainer.class + '">' +
                '</' + this.elementInformationMAP.tagContainer.element + '>');

        this.tagContainerObject = this.containerObject.find('> ' + this.elementInformationMAP.tagContainer.element + '.' + (this.elementInformationMAP.tagContainer.class).replace(' ', '.'));
      },

      assignFilterTriggers   : function() {
        var objectInstance = this,
            triggers       = this.containerObject
                .find(this.triggerInformationMAP.searchTriggerIdentifier);

        jQuery.each(this.triggerInformationMAP.searchTriggerCSSSettings, function(attr, value){
          triggers.css(attr, value);
        });

        triggers.attr('autocomplete', 'off');
        triggers.val('');

        triggers.bind(this.triggerInformationMAP.searchTriggerEvent + '.' + this.namespace, function(event) {
          if(jQuery(this).val().length >= objectInstance.triggerInformationMAP.searchTriggerMinimumLength) {
            if(event.which == 38) {
              objectInstance.KeyNavigationHelper.keyUp();
            } else if(event.which == 40) {
              objectInstance.KeyNavigationHelper.keyDown();
            } else if(event.which == 13) {
              if(objectInstance.KeyNavigationHelper.getCurrentPointedElement() != false)
                objectInstance.KeyNavigationHelper.getCurrentPointedElement().click();

              return false;
            } else if(event.which == 27) {
              triggers.val('');
              objectInstance.ModalHelper.Close();
            } else {
              objectInstance.fetchFilteredResult(jQuery(this).val());
            }
          } else {
            objectInstance.ModalHelper.Close();
          }
        });

        if(this.closeOnUnFocus)
          triggers.focusout(function(){
            setTimeout(function(){
              objectInstance.ModalHelper.Close();
            }, 500);
          });
      },

      fetchFilteredResult : function(search) {
        var objectInstance = this;

        if(this._currentAJAXRequestObject != false)
          this._currentAJAXRequestObject.abort();

        var tagIds = [];

        this.tagContainerObject.find('input[type="' + this.tagInputType + '"][data-tag-id]').each(function(){
          tagIds[tagIds.length] = jQuery(this).val();
        });

        var requestData = this.requestExtraParams;

        requestData[this.requestSearchedTerm] = search;
        requestData[this.requestSelectedTerms] = tagIds;

        this._currentAJAXRequestObject = jQuery.ajax({
          type     : this.requestMethod,
          url      : this.requestURL,
          context  : document.body,
          dataType : 'json',
          data     : requestData
        }).done(function(response) {
          objectInstance.ModalHelper.Close();

          if(response.status == 'empty') {
            if(typeof response.message !== "undefined")
              objectInstance.ModalHelper.DisplayMessage(
                  objectInstance.responseMessageSkeleton.replace('{message}', response.message),
                  objectInstance.containerObject.find(objectInstance.triggerInformationMAP.searchTriggerIdentifier).filter(':first')
              );

            return;
          }

          var modalContent  = objectInstance.buildTagListDisplay(response.results, search),
              displayUnder  = objectInstance.containerObject.find(objectInstance.triggerInformationMAP.searchTriggerIdentifier).filter(':first');

          objectInstance.ModalHelper.Display(modalContent, displayUnder);
          objectInstance._currentAJAXRequestObject = false;
        });
      },

      buildTagListDisplay : function(tagListInformation, searchParam) {
        var entryListHTML = '', objectInstance = this;

        jQuery.each(tagListInformation, function(key, tagInformation){
          entryListHTML += objectInstance.buildTagDisplay(tagInformation, searchParam);
        });

        return this.entryInformationListHTMLSkeleton.replace('{entries_information_list}', entryListHTML);
      },

      buildTagDisplay : function(tagInformation, searchParam) {
        var html = this.entryInformationHTMLSkeleton;


        jQuery.each(tagInformation, function(key, value){
          html = html.replace('{' + key + '|boldSearch}', value.replace(searchParam, '<strong>' + searchParam + '</strong>'));

          html = html.replace(new RegExp('{' + key + '}', 'g'), value);
        });

        return html;
      },

      addTag : function(tagId, tagName) {
        if(this.tagContainerObject.find('[data-tag-id="' + tagId + '"]').length > 0)
          return;

        var objectInstance = this;

        this.tagContainerObject.append(
            '<input type="' + this.tagInputType + '" ' +
                'name="' + this.tagInputName + '[' +
                (this.tagContainerObject.find('[data-tag-id]').length > 0
                    ? ( parseInt(this.tagContainerObject.find('[data-tag-id]:last').attr('data-tag-id'), 10) + 1 )
                    : 1) +
                ']" ' +
                'value="' + tagId + '"' +
                'data-tag-id="' + tagId + '"' +
                '/>'
        );

        this.tagContainerObject.append(
            '<'  + this.elementInformationMAP.tagElement.element + ' ' +
                'class="' + this.elementInformationMAP.tagElement.class + '" ' +
                'data-tag-id="' + tagId + '" ' +
                '>' +
                tagName +
                (
                    '<' + this.elementInformationMAP.tagElementRemove.element + ' ' +
                        'class="' + this.elementInformationMAP.tagElement.class + '" ' +
                        'data-tag-remove-id="' + tagId + '" ' +
                        '>' +
                        this.elementInformationMAP.tagElementRemove.content +
                        '</' + this.elementInformationMAP.tagElementRemove.element + '>'
                    ) +
                '</' + this.elementInformationMAP.tagElement.element + '>');

        this.tagContainerObject.find('[data-tag-remove-id="' + tagId + '"]').bind('click.' + this.namespace, function(){
          objectInstance.removeTag(jQuery(this).attr('data-tag-remove-id'));
        });

        if(this.tagContainerObject.find('input[type="' + this.tagInputType + '"][data-tag-id]').length >= this.maxTags)
          this.containerObject.find(this.triggerInformationMAP.searchTriggerIdentifier).fadeOut('slow');
      },

      removeTag : function(tagId) {
        var objectInstance = this;

        this.tagContainerObject.find('[data-tag-id="' + tagId + '"]').fadeOut('fast', function(){
          jQuery(this).unbind(objectInstance.namespace);
          jQuery(this).remove();
        });

        this.containerObject.find(this.triggerInformationMAP.searchTriggerIdentifier + ':hidden').fadeIn('slow', function(){
          jQuery(this).focus();
        });
      },

      lockFormSubmit : function() {
        if(this.containerObject.is('form'))
          this.containerObject.attr('onkeypress', 'return event.keyCode != 13');
        else
          this.containerObject.parents('form:first').attr('onkeypress', 'return event.keyCode != 13');
      },

      unlockFormSubmit : function() {
        if(this.containerObject.is('form'))
          this.containerObject.attr('onkeypress', '');
        else
          this.containerObject.parents('form:first').attr('onkeypress', '');
      },

      setWindowResizeEvent : function() {
        var objectInstance = this;

        jQuery(window).bind('resize orientationchange', function(){
          objectInstance.ModalHelper.arrangeContainer();
        });
      },

      ModalHelper : {

        /**
         * @var EasyAutoComplete
         */
        Controller      : {},
        container       : false,
        displayUnder    : 0,
        currentElements : false,
        modalIDPrefix   : "jquery-mt-select-",
        modalID         : "",

        Init    : function(controller) {
          this.Controller = controller;

          this.modalID    = this.modalIDPrefix + this.Controller.namespace;

          return jQuery.extend(1, {}, this);
        },

        DisplayMessage : function(message, displayUnder) {
          var objectInstance = this;

          this.displayUnder = displayUnder;
          this.setContainer(message);
          this.arrangeContainer();
          this.Controller.lockFormSubmit();
        },

        Display : function(content, displayUnder) {
          var objectInstance = this;

          this.displayUnder = displayUnder;
          this.setContainer(content);
          this.arrangeContainer();
          this.Controller.lockFormSubmit();

          this.container.find('.addTag').unbind('click').bind('click.' + this.Controller.namespace, function(){
            objectInstance.Controller.addTag(jQuery(this).attr('data-tag-id'), jQuery(this).attr('data-tag-name'));

            if(objectInstance.Controller.clearInputOnSelect == 1) {
              var triggers = objectInstance.Controller.containerObject.find(objectInstance.Controller.triggerInformationMAP.searchTriggerIdentifier);
              triggers.val('');
              triggers.first().focus();
            }

            if(objectInstance.Controller.closeModalOnSelect == 1) {
              objectInstance.Close();
            } else {
              jQuery(this).remove();
              objectInstance.arrangeContainer();
            }
          });
        },

        setContainer  : function(content) {
          jQuery('body').append('<div id="' + this.modalID + '" class="modal-helper">' + content + '</div>');

          this.container = jQuery('#' + this.modalID);

          this.currentElements = this.container.find(this.Controller.entryInformationSingleContainerIdentifier);
        },

        arrangeContainer : function() {
          if(this.container == false)
            return;

          this.container.css('position', 'absolute');
          this.container.css('top',
              this.displayUnder.offset().top +
                  this.displayUnder.height() +
                  parseInt(this.displayUnder.css('padding-top'), 10) +
                  parseInt(this.displayUnder.css('padding-bottom'), 10)
          );
          this.container.css('left', this.displayUnder.offset().left);
        },

        Close : function() {
          if(this.container != false) {
            this.currentElements = false;

            this.Controller.unlockFormSubmit();

            this.container.find('.addTag').unbind('click.' + this.Controller.namespace);
            this.container.remove();
          }

          this.container = false;
        }

      },

      KeyNavigationHelper : {

        Controller : {},

        Init : function(controller) {
          this.Controller = controller;

          return jQuery.extend(1, {}, this);
        },

        keyUp : function() {
          if(this.Controller.ModalHelper.currentElements == false)
            return;

          var currentPointedElement = this.getCurrentPointedElementAndHandleUniversal();

          currentPointedElement = (
              typeof currentPointedElement == "undefined"
                  || currentPointedElement == false
                  || currentPointedElement.prev().length == 0
              )
              ? this.Controller.ModalHelper.currentElements.filter(':last')
              : currentPointedElement.prev();

          currentPointedElement.removeClass('inactive').addClass('active');

          this.scrollToElement(currentPointedElement);
        },

        keyDown : function() {
          if(this.Controller.ModalHelper.currentElements == false)
            return;

          var currentPointedElement = this.getCurrentPointedElementAndHandleUniversal();

          currentPointedElement = (
              typeof currentPointedElement == "undefined"
                  || currentPointedElement == false
                  || currentPointedElement.next().length == 0
              )
              ? this.Controller.ModalHelper.currentElements.filter(':first')
              : currentPointedElement.next();

          currentPointedElement.removeClass('inactive').addClass('active');

          this.scrollToElement(currentPointedElement);
        },

        scrollToElement : function(currentPointedElement) {
          this.Controller.ModalHelper.container.find('> *:first').animate({
            scrollTop: currentPointedElement.position().top
          }, 200);
        },

        getCurrentPointedElementAndHandleUniversal : function() {
          var currentPointedElement = this.getCurrentPointedElement(), objectInstance = this;

          this.Controller.ModalHelper.currentElements.removeClass('active').addClass('inactive');

          this.Controller.ModalHelper.currentElements.unbind('hover').bind('hover', function() {
            objectInstance.Controller.ModalHelper.currentElements.removeClass('active inactive');
          });

          return currentPointedElement;
        },

        getCurrentPointedElement : function() {
          var items = this.Controller.ModalHelper.currentElements;

          return items.filter('.active').length > 0 ? items.filter('.active:first') : false;
        }

      }

    }

  },

  Helpers : {

    Validation : {

      handlers : {

        container : {

          hide : function(inputObject, type, validatorInstance) {
            if(type == 1) {
              jQuery.each(validatorInstance.validationMethods, function(key, validator){
                if(inputObject.is(validator.identifier))
                  jQuery(inputObject.attr(validator.attributeIdentifier)).hide();
              });

            } else {
              jQuery(inputObject.attr(type)).fadeOut('slow');
            }

            inputObject.removeClass(validatorInstance.inputErrorClass);
          },

          show : function(inputObject, type, validatorInstance) {
            jQuery(inputObject.attr(type)).fadeIn('slow');
            jQuery(this).addClass(validatorInstance.inputErrorClass);
          }

        },

        message   : {

          tooltip : {
            attribute           : "data-hint",
            attributeIdentifier : "[data-hint]",
            classes             : "hint--bottom hint--top hint--left hint--right hint--always",
            defaultClass        : "hint--top  hint--error"
          },

          hide : function(inputObject, type, validatorInstance) {
            var parentObject = inputObject.parent();

            if(parentObject.is(this.tooltip.attributeIdentifier))
              parentObject.removeAttr(this.tooltip.attribute);

            inputObject.removeClass(validatorInstance.inputErrorClass);
            parentObject.removeClass(this.tooltip.classes);
          },

          show : function(inputObject, type, validatorInstance) {
            var parentObject = inputObject.parent(),
                message = inputObject.attr(type);

            parentObject.attr(this.tooltip.attribute, message);
            parentObject.addClass(this.tooltip.defaultClass);

            inputObject.addClass(validatorInstance.inputErrorClass);
          }

        }

      },

      validationMethods : {

        required : {
          identifier              : '[data-validation-required]',
          attributeIdentifier     : 'data-validation-required',

          isValid  : function(value, validatorInstance) {
            return jQuery.trim(value) != '';
          }
        },

        email    : {
          identifier              : '[data-validation-email]',
          attributeIdentifier     : 'data-validation-email',

          isValid  : function(value, validatorInstance) {
            return validatorInstance.isValidEmailAddress(value);
          }
        }

      },

      defaultHandler : 'container',

      containerIdentifier   : '[data-validation-container]',
      tooltipIdentifier     : '[data-validation-tooltip]',
      tooltipIdentifierAttr : 'data-validation-tooltip',

      fieldIdentifier         : '',

      inputErrorClass         : 'uff-input-error',

      Init : function() {
        var objectInstance = this;

        var validationMethodsLength = jQuery.map(this.validationMethods, function(n, i) { return i; }).length;

        var i = 1;jQuery.each(this.validationMethods, function(key, validator){
          objectInstance.fieldIdentifier = objectInstance.fieldIdentifier
              + validator.identifier + (i < validationMethodsLength ? ',' : '');
          i++;
        });
      },

      hideErrors     : function(sectionObject) {
        var objectInstance = this;

        sectionObject.find(this.fieldIdentifier).each(function(){
          objectInstance.getHandlerObjectBasedOnInputObject(jQuery(this)).hide(jQuery(this), 1, objectInstance);
        });

      },

      IsValidSection : function(sectionObject) {
        var objectInstance = this,
            ok = true;

        sectionObject.find(this.fieldIdentifier)
            .unbind('keyup.uff_input change.uff_input')
            .each(function(){
              if(objectInstance._validateInput(jQuery(this)) == false) {
                ok = false;

                jQuery(this).bind('keyup.uff_input change.uff_input', function(){
                  objectInstance._validateInput(jQuery(this));
                });
              }
            });

        return ok;
      },

      _validateInput : function(inputObject) {
        var objectInstance = this,
            ok = true;

        jQuery.each(this.validationMethods, function(key, validator){
          if(inputObject.is(validator.identifier)) {
            if(validator.isValid(inputObject.val(), objectInstance)) {
              objectInstance.getHandlerObjectBasedOnInputObject(inputObject).hide(
                  inputObject,
                  validator.attributeIdentifier,
                  objectInstance
              );
            } else {
              ok = false;
              objectInstance.getHandlerObjectBasedOnInputObject(inputObject).show(
                  inputObject,
                  validator.attributeIdentifier,
                  objectInstance
              );
              return false;
            }
          }
        });

        return ok;
      },

      getHandlerObjectBasedOnInputObject : function(inputObject) {
        if(inputObject.is(this.tooltipIdentifier))
          return this.handlers.message;
        else if(inputObject.is(this.containerIdentifier))
          return this.handlers.container;
        else
          return this.handlers[this.defaultHandler];
      },

      isValidEmailAddress : function(emailAddress) {
        var pattern = new RegExp(/^((([a-z]|\d|[!#\$%&'\*\+\-\/=\?\^_`{\|}~]|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])+(\.([a-z]|\d|[!#\$%&'\*\+\-\/=\?\^_`{\|}~]|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])+)*)|((\x22)((((\x20|\x09)*(\x0d\x0a))?(\x20|\x09)+)?(([\x01-\x08\x0b\x0c\x0e-\x1f\x7f]|\x21|[\x23-\x5b]|[\x5d-\x7e]|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])|(\\([\x01-\x09\x0b\x0c\x0d-\x7f]|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF]))))*(((\x20|\x09)*(\x0d\x0a))?(\x20|\x09)+)?(\x22)))@((([a-z]|\d|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])|(([a-z]|\d|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])([a-z]|\d|-|\.|_|~|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])*([a-z]|\d|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])))\.)+(([a-z]|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])|(([a-z]|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])([a-z]|\d|-|\.|_|~|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])*([a-z]|[\u00A0-\uD7FF\uF900-\uFDCF\uFDF0-\uFFEF])))\.?$/i);
        return pattern.test(emailAddress);
      }

    }

  }

};

jQuery(document).ready(function(){
  jQuery('.component-uff').uff();

});

jQuery.fn.extend({
  uff : function () {
    jQuery(this).each(function(){
      var instance = jQuery.extend(true, {}, jQueryUFF);

      instance.Init(jQuery(this));
    });

    return jQuery(this);
  }
});

