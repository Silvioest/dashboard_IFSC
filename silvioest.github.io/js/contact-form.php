<div class="row">
    <div class="col-md-6">
        <div id="contact" class="form-container">
            <fieldset>
                <div id="message"></div>
                <form method="post" action="js/contact-form.php" name="contactform" id="contactform">
                    <div class="form-group">
                        <input name="name" id="name" type="text" value="" placeholder="Name" class="form-control" />
                    </div>
                    <div class="form-group">
                        <input name="email" id="email" type="text" value="" placeholder="Email" class="form-control" />
                    </div>
                    <div class="form-group">
                        <input name="phone" id="phone" type="text" value="" placeholder="Phone" class="form-control" />
                    </div>
                    <div class="form-group">
                        <textarea name="comments" id="comments" class="form-control" rows="3" placeholder="Message" id="textArea"></textarea>
                        <div class="editContent">
                            <p class="small text-muted"><span class="guardsman">* All fields are required.</span> Once we receive your message we will respond as soon as possible.</p>
                        </div>
                    </div>
                    <div class="form-group">
                        <button class="btn btn-primary" type="submit" id="cf-submit" name="submit">Send</button>
                    </div>
                </form>
            </fieldset>
        </div>
        <!-- /.form-container -->
    </div>
    <div class="col-md-6">
        <h2>How to find us</h2>
        <p>We are easy to find in the heart of London's business district, just a short walk from several Tube Stations.</p>
        <img class="img-responsive map-img" src="http://placehold.it/600x300">
    </div>
</div>
